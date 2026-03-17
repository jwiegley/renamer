{
  description = "Renamer - EXIF-based photo file renaming tool";

  inputs = {
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    haskellNix.url = "github:input-output-hk/haskell.nix";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system overlays;
          inherit (haskellNix) config;
        };
        flake = pkgs.renamer.flake { };
        overlays = [
          haskellNix.overlay
          (final: prev: {
            haskellPackages = prev.haskellPackages.override {
              overrides = hfinal: hprev: {
                haskell-language-server =
                  hprev.disableCabalFlag "ormolu" (
                    hprev.disableCabalFlag "fourmolu" (
                      hprev.haskell-language-server.override {
                        configureFlags =
                          [ "--flag=-ormolu" "--flag=-fourmolu" ];
                        hls-ormolu-plugin = null;
                        hls-fourmolu-plugin = null;
                      }));
              };
            };
            renamer = final.haskell-nix.project' {
              src = ./.;
              supportHpack = true;
              compiler-nix-name = "ghc910";
              shell = {
                tools = {
                  cabal = { };
                  haskell-language-server = { };
                  ghcid = { };
                };
                buildInputs = with final; [
                  pkg-config
                  exiftool
                  lefthook
                  haskellPackages.fourmolu
                  haskellPackages.hlint
                ];
                withHoogle = true;
              };
              modules = [{
                enableLibraryProfiling = true;
                enableProfiling = true;
              }];
            };
          })
        ];

        cleanSrc = pkgs.lib.cleanSource ./.;
        fourmolu = pkgs.haskellPackages.fourmolu;
        hlint = pkgs.haskellPackages.hlint;

      in flake // {
        packages = flake.packages // {
          default = flake.packages."renamer:exe:renamer";

          # Format all Haskell source files: nix run .#format
          format = pkgs.writeShellScriptBin "renamer-format" ''
            exec ${fourmolu}/bin/fourmolu --mode inplace \
              $(${pkgs.findutils}/bin/find src bin tests -name '*.hs')
          '';
        };

        checks = (flake.checks or { }) // {
          # Verify all Haskell source files are correctly formatted
          formatting = pkgs.runCommand "check-formatting" {
            nativeBuildInputs = [ fourmolu pkgs.findutils ];
          } ''
            cd ${cleanSrc}
            find . -name '*.hs' \
              -exec fourmolu --mode check --config fourmolu.yaml {} +
            touch $out
          '';

          # Verify all Haskell source files pass linting
          lint = pkgs.runCommand "check-lint" {
            nativeBuildInputs = [ hlint pkgs.findutils ];
          } ''
            cd ${cleanSrc}
            find . -name '*.hs' | xargs hlint
            touch $out
          '';

          # Verify the test suite passes
          tests = flake.packages."renamer:test:renamer-tests";
        };
      });
}
