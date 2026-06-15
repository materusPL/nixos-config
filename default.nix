let
  flake-compatish = import (
    builtins.fetchTree {
      type = "github";
      owner = "lillecarl";
      repo = "flake-compatish";
      ref = "main";
    }
  );

  flake = flake-compatish {
    source = ./.;
    overrides = {
      self = ./.;
      materus-inputs = ./nix-config/subflake;
      materus-inputs-unstable = ./nix-config/subflake;
    };
  };
in
flake.impure

# sudo -i nixos-rebuild switch --file . --attr nixosConfigurations.${HOSTNAME}
