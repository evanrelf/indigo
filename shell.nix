let
  pkgs = import <nixpkgs> {};

in
  pkgs.mkShell {
    buildInputs = with pkgs; [
      cargo
      clippy
      rustc
      rustfmt
    ];
  }
