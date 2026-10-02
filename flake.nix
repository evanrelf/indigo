{
  inputs = {
    crane.url = "github:ipetkov/crane";
    fenix = {
      url = "github:nix-community/fenix/monthly";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    flake-parts = {
      url = "github:hercules-ci/flake-parts";
      inputs.nixpkgs-lib.follows = "nixpkgs";
    };
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";
    systems.url = "github:nix-systems/default";
  };

  outputs = inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = import inputs.systems;

      perSystem = { config, inputs', lib, pkgs, system, ... }:
        let
          crane =
            (inputs.crane.mkLib pkgs).overrideToolchain
              inputs'.fenix.packages.default.toolchain;

          craneStatic =
            (inputs.crane.mkLib pkgs.pkgsStatic).overrideToolchain (_:
              inputs'.fenix.packages.combine [
                inputs'.fenix.packages.default.toolchain
                inputs'.fenix.packages.targets.${pkgs.pkgsStatic.stdenv.hostPlatform.rust.rustcTarget}.latest.rust-std
              ]);

          commonArgs = {
            pname = "indigo";
            version = "0.0.0";
            src = crane.cleanCargoSource ./.;
            strictDeps = true;
          };

          cargoArtifacts = crane.buildDepsOnly commonArgs;
        in
        {
          packages = {
            default =
              crane.buildPackage (commonArgs // {
                inherit cargoArtifacts;
                cargoExtraArgs = "--bin indigo";
              });

            docs = config.checks.doc;
          } // lib.optionalAttrs pkgs.stdenv.hostPlatform.isLinux {
            static =
              craneStatic.buildPackage (commonArgs // {
                cargoArtifacts = craneStatic.buildDepsOnly commonArgs;
                cargoExtraArgs = "--bin indigo";
              });
          };

          checks.clippy =
            crane.cargoClippy (commonArgs // { inherit cargoArtifacts; });

          checks.doc =
            crane.cargoDoc (commonArgs // {
              inherit cargoArtifacts;
              # Generate a root `index.html` listing all crates (nightly only)
              RUSTDOCFLAGS = "-Zunstable-options --enable-index-page";
            });

          checks.test =
            crane.cargoTest (commonArgs // { inherit cargoArtifacts; });

          devShells.default =
            crane.devShell {
              checks = config.checks;
              packages = with pkgs; [
                cargo-fuzz
                kakoune
              ];
            };
        };
    };
}
