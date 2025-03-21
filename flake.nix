{
  description = "migaman";

  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
  };

  outputs = inputs:
    inputs.flake-parts.lib.mkFlake {inherit inputs;} ({
      flake-parts-lib,
      moduleWithSystem,
      ...
    }: {
      systems = [
        "aarch64-darwin"
        "aarch64-linux"
        "x86_64-darwin"
        "x86_64-linux"
      ];
      perSystem = {
        pkgs,
        system,
        config,
        ...
      }: let
        pkgs = inputs.nixpkgs.legacyPackages.${system};
        pkgsStatic = inputs.nixpkgs.legacyPackages.${system}.pkgsMusl;
        myOverlay = pkgs.haskell.lib.compose.packageSourceOverrides {
          migaman = ./.;
          migadu = ./migadu;
        };
        hsPkgs = pkgs.haskell.packages.ghc982.extend myOverlay;
        fixGhc = pkg: pkg.override {
          enableRelocatedStaticLibs = true;
          enableShared = false;
          enableDwarf = false;
        };
        hsPkgsStatic = (pkgsStatic.haskell.packages.ghc982.override (old: {
          ghc = fixGhc old.ghc;
          buildHaskellPackages = old.buildHaskellPackages.override (oldBHP: {
            ghc = fixGhc oldBHP.ghc;
          });
        })).extend myOverlay;

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
          ];
        };

        packages.migaman = pkgs.haskell.lib.justStaticExecutables hsPkgs.migaman;
        packages.migaman-static = pkgs.haskell.lib.overrideCabal hsPkgsStatic.migaman (old: {
          postInstall = (old.postInstall or "") + ''
            for b in $out/bin/*
            do
              if ldd "$b"
              then
                echo "ldd succeeded on $b, which may mean that it is not statically linked"
                exit 1
              fi
            done
          '';
          configureFlags = (old.configureFlags or [ ]) ++ [
            "--ghc-option=-optl=-static"
            "--extra-lib-dirs=${pkgsStatic.gmp6.override { withStatic = true; }}/lib"
            "--extra-lib-dirs=${pkgsStatic.libffi.overrideAttrs (old: { dontDisableStatic = true; })}/lib"
            "--extra-lib-dirs=${pkgsStatic.zlib.static}/lib"
          ];
          enableSharedExecutables = false;
          enableSharedLibraries = false;
        });

        packages.default = config.packages.migaman;

        formatter = pkgs.alejandra;
      };
      flake = {config, ...}: {
        homeManagerModules = {
          migaman = moduleWithSystem (
            perSystem @ {config}: flake-parts-lib.importApply ./nix/migaman.nix perSystem
          );
          default = config.homeManagerModules.migaman;
        };
      };
    });
}
