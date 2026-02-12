{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/master";
    flake-utils.url = "github:numtide/flake-utils";
    fenix = {
      url = "github:nix-community/fenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    lutra-pygments = {
      url = "git+https://codeberg.org/lutra/lutra-pygments.git?ref=main";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      fenix,
      lutra-pygments,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
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
        fenix_pkgs = fenix.packages.${system};
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = [
            (fenix_pkgs.stable.withComponents [
              "cargo"
              "clippy"
              "rust-src"
              "rustc"
              "rustfmt"
              "rust-analyzer"
            ])

            pkgs.just
            pkgs.cargo-nextest
            pkgs.cargo-insta
            pkgs.cargo-depgraph
            pkgs.graphviz

            pkgs.pgcli
            pkgs.postgresql # for psql only (lutra-runner-python tests need it)
            pkgs.duckdb # for lutra-runner-duckdb (system library)

            # python
            pkgs.python312Packages.python
            pkgs.python312Packages.venvShellHook
            pkgs.uv
            pkgs.maturin
            pkgs.ruff
            pkgs.mypy

            pkgs.python312Packages.python-lsp-server
            # pkgs.python312Packages.python-lsp-ruff
            # pkgs.python312Packages.pylsp-mypy
            # to make mypy pick up packages from venv, run `uv pip install mypy`

            # website
            pkgs.mprocs
            pkgs.rsync
            pkgs.zola
            pkgs.typos-lsp
            pkgs.zensical
          ];
          LD_LIBRARY_PATH = "${pkgs.lib.makeLibraryPath [ pkgs.duckdb ]}";
          venvDir = "./target/python";
        };
      }
    );
}
