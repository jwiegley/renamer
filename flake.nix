{
  description = "Tool to rename photos";

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
        flake = pkgs.renamer.flake {
        };
        overlays = [ haskellNix.overlay
          (final: prev: {
            haskellPackages = prev.haskellPackages.override {
              overrides = hfinal: hprev: {
                haskell-language-server =
                  hprev.disableCabalFlag "ormolu" (
                  hprev.disableCabalFlag "fourmolu" (
                    hprev.haskell-language-server.override {
                      configureFlags = [ "--flag=-ormolu" "--flag=-fourmolu" ];
                      hls-ormolu-plugin = null;
                      hls-fourmolu-plugin = null;
                    }
                  ));
              };
            };
            renamer =
              final.haskell-nix.project' {
                src = ./.;
                supportHpack = true;
                compiler-nix-name = "ghc910";
                shell = {
                  tools = {
                    cabal = {};
                    haskell-language-server = {};
                    # hlint = {};
                    ghcid = {};
                  };
                  buildInputs = with pkgs; [
                    pkg-config
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
      in flake // {
        packages.default = flake.packages."renamer:exe:renamer";
      });
}
