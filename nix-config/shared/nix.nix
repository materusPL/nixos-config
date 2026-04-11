{
  materusArgs,
  lib,
  pkgs,
  ...
}:
{
  config.nix.package = lib.mkDefault pkgs.nixVersions.latest;
  config.nix.registry = {
    nixpkgs-stable = {
      from = {
        type = "indirect";
        id = "nixpkgs-stable";
      };
      flake = materusArgs.self.inputs.nixpkgs;
    };
    nixpkgs-unstable = {
      from = {
        type = "indirect";
        id = "nixpkgs-unstable";
      };
      flake = materusArgs.self.inputs.nixpkgs-unstable;
    };

    nixpkgs = {
      from = {
        type = "indirect";
        id = "nixpkgs";
      };
      flake = materusArgs.inputs.nixpkgs;
    };

    emacs-overlay = {
      from = {
        type = "indirect";
        id = "emacs-overlay";
      };
      flake = materusArgs.inputs.emacs-overlay;
    };

    flake-utils = {
      from = {
        type = "indirect";
        id = "flake-utils";
      };
      flake = materusArgs.inputs.flake-utils;
    };

    nixos-hardware = {
      from = {
        type = "indirect";
        id = "nixos-hardware";
      };
      flake = materusArgs.inputs.nixos-hardware;
    };

    nixerus = {
      from = {
        type = "indirect";
        id = "nixerus";
      };
      flake = materusArgs.inputs.nixerus;
    };

    devshell = {
      from = {
        type = "indirect";
        id = "devshell";
      };
      flake = materusArgs.inputs.devshell;
    };

    home-manager = {
      from = {
        type = "indirect";
        id = "home-manager";
      };
      flake = materusArgs.inputs.home-manager;
    };

    sops-nix = {
      from = {
        type = "indirect";
        id = "sops-nix";
      };
      flake = materusArgs.inputs.sops-nix;
    };

    base16 = {
      from = {
        type = "indirect";
        id = "base16";
      };
      flake = materusArgs.inputs.base16;
    };

    git-agecrypt = {
      from = {
        type = "indirect";
        id = "git-agecrypt";
      };
      flake = materusArgs.inputs.git-agecrypt;
    };

    plasma-manager = {
      from = {
        type = "indirect";
        id = "plasma-manager";
      };
      flake = materusArgs.inputs.plasma-manager;
    };

    nur = {
      from = {
        type = "indirect";
        id = "nur";
      };
      flake = materusArgs.inputs.nur;
    };

    nix-vscode-extensions = {
      from = {
        type = "indirect";
        id = "nix-vscode-extensions";
      };
      flake = materusArgs.inputs.nix-vscode-extensions;
    };

  };
}
