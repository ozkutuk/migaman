{
  description = "migaman";

  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
  };

  outputs = inputs:
    inputs.flake-parts.lib.mkFlake {inherit inputs;} {
      systems = [
        "aarch64-darwin"
        "aarch64-linux"
        "x86_64-darwin"
        "x86_64-linux"
      ];
      perSystem = {
        pkgs,
        system,
        ...
      }: let
        pkgs = inputs.nixpkgs.legacyPackages.${system};
        myOverlay = pkgs.haskell.lib.compose.packageSourceOverrides {
          migaman = ./.;
          migadu = ./migadu;
        };
        hsPkgs = pkgs.haskell.packages.ghc982.extend myOverlay;

      in {
        devShells.default = hsPkgs.shellFor {
          packages = p: [p.migaman p.migadu];
          nativeBuildInputs = [
            hsPkgs.cabal-install
            hsPkgs.haskell-language-server
            hsPkgs.fourmolu
            hsPkgs.cabal-fmt

            pkgs.just
            pkgs.sqlite
            pkgs.sqlitebrowser
            pkgs.dbmate
          ];
        };

        packages.default = hsPkgs.migaman;

        formatter = pkgs.alejandra;
      };
    };
}
