{
  description = "All the flakes";
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

    home-manager = {
      type = "github";
      owner = "nix-community";
      repo = "home-manager";
      ref = "master";
      inputs = {
        nixpkgs.follows = "nixpkgs";
      };
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

    nix-vscode-extensions = {
      type = "github";
      owner = "nix-community";
      repo = "nix-vscode-extensions";
      ref = "master";
      inputs = {
        nixpkgs.follows = "nixpkgs";
      };
    };

  };

  outputs = inputs: inputs;
}
