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

      perSystem = { pkgs, system, ... }: {
        packages.default =
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
          crane.buildPackage (commonArgs // {
            inherit cargoArtifacts;
            cargoExtraArgs = "--bin indigo";
          });
      };
    };
}
