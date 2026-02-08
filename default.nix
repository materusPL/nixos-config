let
  flake-compatish = import (
    builtins.fetchTree {
      type = "github";
      owner = "lillecarl";
      repo = "flake-compatish";
      ref = "main";
    }
  );

  flake = flake-compatish ./.;
in
flake.impure

# sudo -i nixos-rebuild switch --file . --attr nixosConfigurations.${HOSTNAME}
