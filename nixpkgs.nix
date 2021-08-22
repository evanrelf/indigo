let
  # master as of 2021-08-17
  rev = "3788c68def67ca7949e0864c27638d484389363d";

  sha256 = "1i65q4rxrr63sy5mnkbyq3li185j3fl456lwxv3c9cw1c61b8g82";

  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    inherit sha256;
  };

in
import nixpkgs {
  config = { };
}
