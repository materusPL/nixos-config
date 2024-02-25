{
  description = "Materus hosts and user config";
  inputs = {
    nixpkgs = {
      type = "github";
      owner = "NixOS";
      repo = "nixpkgs";
      ref = "nixos-unstable";
    };

    flake-utils = {
      type = "github";
      owner = "numtide";
      repo = "flake-utils";
      ref = "main";
    };

    base16 = {
      type = "github";
      owner = "SenchoPens";
      repo = "base16.nix";
      ref = "main";
    };

    devshell = {
      type = "github";
      owner = "numtide";
      repo = "devshell";
      ref = "main";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
      };
    };
    
    home-manager = {
      type = "github";
      owner = "nix-community";
      repo = "home-manager";
      ref = "master";
      inputs = {
        nixpkgs.follows = "nixpkgs";
      };
    };

    emacs-overlay = {
      type = "github";
      owner = "nix-community";
      repo = "emacs-overlay";
      ref = "master";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
      };
    };

    nixos-hardware = {
      type = "github";
      owner = "NixOS";
      repo = "nixos-hardware";
      ref = "master";
    };

    nixerus = {
      type = "github";
      owner = "materusPL";
      repo = "Nixerus";
      ref = "master";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        home-manager.follows = "home-manager";
      };
    };

    sops-nix = {
      type = "github";
      owner = "Mic92";
      repo = "sops-nix";
      ref = "master";
      inputs = {
        nixpkgs.follows = "nixpkgs";
      };
    };

    git-agecrypt = {
      type = "github";
      owner = "vlaci";
      repo = "git-agecrypt";
      ref = "main";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
      };
    };

    nur = {
      type = "github";
      owner = "nix-community";
      repo = "NUR";
      ref = "master";
    };

  };


  outputs = inputs @ { self, nixpkgs, home-manager, nur, ... }:
    {

      inherit inputs;

    };
}
