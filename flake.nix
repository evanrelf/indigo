{
  description = "indigo";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:NixOS/nixpkgs";
  };

  outputs = inputs@{ flake-utils, nixpkgs, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };

      in
      rec {
        packages = {
          default = packages.indigo;

          indigo = pkgs.haskellPackages.callCabal2nix "indigo" ./. { };
        };

        devShells = {
          default = devShells.indigo;

          indigo = packages.indigo.env.overrideAttrs (prev: {
            buildInputs = (prev.buildInputs or [ ]) ++ [
              pkgs.cabal-install
              pkgs.ghcid
            ];
          });
        };
      }
    );
}
