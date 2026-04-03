{...}:{
config.nix.package = lib.mkDefault  pkgs.nixVersions.latest;
  config.nix.registry = lib.mkIf config.materus.profile.nix.enableRegistry {
    nixpkgs-stable = {
      from = { type = "indirect"; id = "nixpkgs-stable"; };
      flake = materusCfg.materusFlake.inputs.nixpkgs-stable;
    };
    nixpkgs-unstable = {
      from = { type = "indirect"; id = "nixpkgs-unstable"; };
      flake = materusCfg.materusFlake.inputs.nixpkgs;
    };

    nixpkgs = {
      from = { type = "indirect"; id = "nixpkgs"; };
      flake = materusCfg.configInputs.nixpkgs;
    };

    emacs-overlay = {
      from = { type = "indirect"; id = "emacs-overlay"; };
      flake = materusCfg.configInputs.emacs-overlay;
    };

    flake-utils = {
      from = { type = "indirect"; id = "flake-utils"; };
      flake = materusCfg.configInputs.flake-utils;
    };

    nixos-hardware = {
      from = { type = "indirect"; id = "nixos-hardware"; };
      flake = materusCfg.configInputs.nixos-hardware;
    };

    nixerus = {
      from = { type = "indirect"; id = "nixerus"; };
      flake = materusCfg.configInputs.nixerus;
    };

    devshell = {
      from = { type = "indirect"; id = "devshell"; };
      flake = materusCfg.configInputs.devshell;
    };

    home-manager = {
      from = { type = "indirect"; id = "home-manager"; };
      flake = materusCfg.configInputs.home-manager;
    };

    sops-nix = {
      from = { type = "indirect"; id = "sops-nix"; };
      flake = materusCfg.configInputs.sops-nix;
    };

    base16 = {
      from = { type = "indirect"; id = "base16"; };
      flake = materusCfg.configInputs.base16;
    };

    git-agecrypt = {
      from = { type = "indirect"; id = "git-agecrypt"; };
      flake = materusCfg.configInputs.git-agecrypt;
    };

    plasma-manager = {
      from = { type = "indirect"; id = "plasma-manager"; };
      flake = materusCfg.configInputs.plasma-manager;
    };

    nur = {
      from = { type = "indirect"; id = "nur"; };
      flake = materusCfg.configInputs.nur;
    };
    nix-vscode-extensions = {
      from = { type = "indirect"; id = "nix-vscode-extensions"; };
      flake = materusCfg.configInputs.nix-vscode-extensions;
    };

  };
  }