let
  pkgs = import ./nixpkgs.nix;

in
pkgs.mkShell {
  buildInputs = with pkgs; [
    cargo
    clippy
    libiconv
    rustc
    rustfmt
  ];
}
