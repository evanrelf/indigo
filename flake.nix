{
  inputs = {
    crane.url = "github:ipetkov/crane";
    flake-parts = {
      url = "github:hercules-ci/flake-parts";
      inputs.nixpkgs-lib.follows = "nixpkgs";
    };
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11";
    systems.url = "github:nix-systems/default";
  };

  outputs = inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = import inputs.systems;

      perSystem = { config, pkgs, system, ... }:
        let
          crane = inputs.crane.mkLib pkgs;

          commonArgs = {
            pname = "indigo";
            version = "0.0.0";
            src = crane.cleanCargoSource ./.;
            strictDeps = true;
          };

          cargoArtifacts = crane.buildDepsOnly commonArgs;
        in
        {
          packages.default =
            crane.buildPackage (commonArgs // {
              inherit cargoArtifacts;
              cargoExtraArgs = "--bin indigo";
            });

          checks.clippy =
            crane.cargoClippy (commonArgs // { inherit cargoArtifacts; });

          checks.doc =
            crane.cargoDoc (commonArgs // { inherit cargoArtifacts; });

          checks.test =
            crane.cargoTest (commonArgs // { inherit cargoArtifacts; });

          devShells.default =
            crane.devShell {
              checks = config.checks;
            };
        };
    };
}
