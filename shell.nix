let
  pkgs = import <nixpkgs> { };

  indigo = pkgs.haskellPackages.callCabal2nix "indigo" ./. { };

in
indigo.env.overrideAttrs (prev: {
  buildInputs = [
    pkgs.cabal-install
    pkgs.ghcid
  ];
})
