{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    fenix = {
      url = "github:nix-community/fenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, flake-utils, fenix }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
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
            pkgs.clang
            pkgs.mold

            pkgs.just
            pkgs.cargo-nextest
            pkgs.cargo-insta
            pkgs.cargo-depgraph
            pkgs.graphviz

            # python
            pkgs.python312Packages.python
            pkgs.python312Packages.venvShellHook
            pkgs.uv
            pkgs.maturin
            pkgs.ruff
            pkgs.mypy

            pkgs.python312Packages.python-lsp-server
            pkgs.python312Packages.python-lsp-ruff

            # website
            pkgs.cobalt
          ];

          RUSTFLAGS = "-Clink-arg=-fuse-ld=${pkgs.mold}/bin/mold";
          venvDir = "./target/python";
        };
      });
}
