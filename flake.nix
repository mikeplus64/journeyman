{
  description = "journeyman";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    treefmt-nix.url = "github:numtide/treefmt-nix";
    treefmt-nix.inputs.nixpkgs.follows = "nixpkgs";
    flake-root.url = "github:srid/flake-root";
    mission-control.url = "github:Platonic-Systems/mission-control";
  };

  outputs = inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" ];
      imports = [
        inputs.haskell-flake.flakeModule
        inputs.treefmt-nix.flakeModule
        inputs.flake-root.flakeModule
      ];
      perSystem = { self', system, lib, config, pkgs, ... }: {
        # Our only Haskell project. You can have multiple projects, but this template
        # has only one.
        # See https://github.com/srid/haskell-flake/blob/master/example/flake.nix
        haskellProjects.default = {
          # The base package set (this value is the default)
          # basePackages = pkgs.haskellPackages;

          # Packages to add on top of `basePackages`
          packages = {
            # Add source or Hackage overrides here
            # (Local packages are added automatically)
            /*
            aeson.source = "1.5.0.0" # Hackage version
            */
          };

          # Add your package overrides here
          settings = {
            journeyman.check = true;
          };

          # Development shell configuration
          devShell = {
            hlsCheck.enable = true;
            hlsCheck.tools = [ "cabal-fmt" "implicit-hie" ];
          };

          # What should haskell-flake add to flake outputs?
          autoWire = [ "packages" "apps" "checks" ]; # Wire all but the devShell
        };

        # Auto formatters. This also adds a flake check to ensure that the
        # source tree was auto formatted.
        treefmt.config = {
          inherit (config.flake-root) projectRootFile;
          package = pkgs.treefmt;
          programs.ormolu.enable = false;
          programs.nixpkgs-fmt.enable = true;
          programs.cabal-fmt.enable = true;
          programs.hlint.enable = true;
          # programs.ormolu.package = pkgs.haskellPackages.fourmolu;
        };

        # Default package & app.
        packages.default = self'.packages.dat;
        apps.default = self'.apps.dat;

        # Default shell.
        devShells.default = pkgs.mkShell {
          name = "journeyman";
          nativeBuildInputs = [ pkgs.just config.treefmt.build.wrapper ];
          # See https://haskell.flake.page/devshell#composing-devshells
          inputsFrom = [
            config.haskellProjects.default.outputs.devShell
            config.flake-root.devShell
            config.treefmt.build.devShell
          ];
        };
      };
    };
}
