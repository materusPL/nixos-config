{
  description = "MKK flake: NixOS and home manager configs";
  inputs = {
    nixpkgs = {
      type = "github";
      owner = "NixOS";
      repo = "nixpkgs";
      ref = "nixos-unstable";
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
    
    config-unstable = {
      url = "path:./nix/subflake";
      inputs = {
        nixpkgs.follows = "nixpkgs";
      };
    };
    
    config-stable = {
      url = "path:./nix/subflake";
      inputs = {
        nixpkgs.follows = "nixpkgs-stable";
        home-manager.follows = "hm-stable";
      };
    };
  };
  outputs = inputs: import (builtins.toString ./nix) { inherit inputs; configRootPath = (builtins.toString ./.); };

}
