# [[file:org-conf/nix-flake.org::*Main Flake][Main Flake:1]]
{
  description = "Lemon flake: NixOS and home manager configs";
  inputs = {
    private = {
      type = "github";
      owner = "materusPL";
      repo = "Nixerus";
      ref = "mock";
    };

    nixpkgs = {
      type = "github";
      owner = "NixOS";
      repo = "nixpkgs";
      ref = "nixos-unstable";
    };

    configInputs = {
      type = "github";
      owner = "materusPL";
      repo = "nixos-config";
      ref = "inputs";
      inputs = {
        nixpkgs.follows = "nixpkgs";
      };
    };

    nixpkgs-stable = {
      type = "github";
      owner = "NixOS";
      repo = "nixpkgs";
      ref = "nixos-24.11";
    };

    hm-stable = {
      type = "github";
      owner = "nix-community";
      repo = "home-manager";
      ref = "release-24.11";
      inputs.nixpkgs.follows = "nixpkgs-stable";
    };

    configInputs-stable = {
      type = "github";
      owner = "materusPL";
      repo = "nixos-config";
      ref = "inputs";
      inputs = {
        nixpkgs.follows = "nixpkgs-stable";
        home-manager.follows = "hm-stable";
      };
    };
  };
  outputs =
    let
      rootPath = builtins.toString ./.;
    in
    inputs@{ self, ... }:
    {

    };
}
# Main Flake:1 ends here
