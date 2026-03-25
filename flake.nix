{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/master";
    flake-utils.url = "github:numtide/flake-utils";
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    lutra-pygments = {
      url = "git+https://codeberg.org/lutra/lutra-pygments.git?ref=main";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    flake-compat = {
      url = "github:NixOS/flake-compat";
      flake = false;
    };
    nix-appimage = {
      url = "github:ralismark/nix-appimage";
    };
  };

  outputs =
    {
      nixpkgs,
      flake-utils,
      rust-overlay,
      lutra-pygments,
      nix-appimage,
      ...
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            rust-overlay.overlays.default
            # overlay zensical to include lutra-pygments
            (final: prev: {
              zensical = prev.zensical.overridePythonAttrs (old: {
                dependencies = old.dependencies ++ [
                  lutra-pygments.packages.${system}.default
                ];
              });
            })
          ];
        };

        # dev shell
        devShell = pkgs.mkShell {
          buildInputs = [
            (pkgs.rust-bin.fromRustupToolchainFile ./rust-toolchain.toml)
            pkgs.duckdb # for lutra-runner-duckdb

            # tools
            pkgs.just
            pkgs.usql
            pkgs.cargo-nextest
            pkgs.cargo-insta
            pkgs.cargo-depgraph
            pkgs.graphviz
            pkgs.podman
            pkgs.samply

            # python
            pkgs.python312Packages.python
            pkgs.python312Packages.venvShellHook
            pkgs.maturin
            pkgs.ruff
            pkgs.ty
            pkgs.uv

            (pkgs.python312Packages.python-lsp-server.overridePythonAttrs (old: {
              dependencies = builtins.filter (dep: (dep.pname or "") != "autopep8") old.dependencies;
            }))
            # Note: install mypy in the venv instead: `uv pip install mypy`

            # website
            pkgs.mprocs
            pkgs.rsync
            pkgs.zola
            pkgs.typos-lsp
            # Wrap zensical to prevent its python 3.13 propagated-build-inputs
            # from leaking into PYTHONPATH (which breaks pyo3/maturin builds
            # that use the python 3.12 venv).
            (pkgs.symlinkJoin {
              name = "zensical-wrapped";
              paths = [ pkgs.zensical ];
              postBuild = "rm -rf $out/nix-support";
            })
          ];
          LD_LIBRARY_PATH = "${pkgs.lib.makeLibraryPath [ pkgs.duckdb ]}";
          venvDir = "./target/python";
        };

        # lutra-cli binary
        lutra-cli = pkgs.rustPlatform.buildRustPackage {
          pname = "lutra-cli";
          version = "0.4.0";

          src = ./.;

          cargoLock = {
            lockFile = ./Cargo.lock;
          };

          buildInputs = [
            pkgs.duckdb.lib
          ];

          # Build only the lutra-cli binary from workspace
          cargoBuildFlags = [
            "-p"
            "lutra-cli"
          ];

          # Skip tests - they require a running PostgreSQL instance
          doCheck = false;

          meta = {
            description = "Lutra query language CLI";
            homepage = "https://lutra-lang.org";
            mainProgram = "lutra";
          };
        };
      in
      {
        packages = {
          default = lutra-cli;
          lutra-cli = lutra-cli;

          lutra-cli-appimage = nix-appimage.bundlers.${system}.default lutra-cli;

          dev-container = pkgs.dockerTools.buildNixShellImage {
            drv = devShell;
            tag = "latest";
          };
        };

        devShells.default = devShell;
      }
    );
}
