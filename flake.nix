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

      perSystem = { config, inputs', pkgs, system, ... }:
        let
          crane =
            (inputs.crane.mkLib pkgs).overrideToolchain
              inputs'.fenix.packages.default.toolchain;

          commonArgs = {
            pname = "indigo";
            version = "0.0.0";
            src = crane.cleanCargoSource ./.;
            strictDeps = true;
          };

          cargoArtifacts = crane.buildDepsOnly commonArgs;

          toolchainWasm =
            with inputs'.fenix.packages; combine [
              minimal.cargo
              minimal.rustc
              targets.wasm32-wasip1.latest.rust-std
            ];

          craneWasm = crane.overrideToolchain toolchainWasm;

          commonArgsWasm = commonArgs // {
            CARGO_BUILD_TARGET = "wasm32-wasip1";
            cargoExtraArgs = "--package indigo-kernel";
            # Fenix does not fix up the rpath of `rust-lld` on Darwin, so it cannot
            # find `libLLVM.dylib` on its own.
            DYLD_FALLBACK_LIBRARY_PATH = "${toolchainWasm}/lib";
            doCheck = false;
          };

          cargoArtifactsWasm = craneWasm.buildDepsOnly commonArgsWasm;
        in
        {
          packages.default = config.packages.indigo;

          packages.indigo =
            crane.buildPackage (commonArgs // {
              inherit cargoArtifacts;
              cargoExtraArgs = "--bin indigo";
            });

          packages.indigo-kernel-wasm =
            craneWasm.buildPackage (commonArgsWasm // {
              pname = "indigo-kernel";
              cargoArtifacts = cargoArtifactsWasm;
            });

          packages.docs = config.checks.doc;

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
                wasmtime
              ];
            };
        };
    };
}
