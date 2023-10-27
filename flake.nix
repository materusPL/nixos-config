{
  description = "Materus hosts and user config";
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

    nixpkgs-stable = {
      type = "github";
      owner = "NixOS";
      repo = "nixpkgs";
      ref = "nixos-23.05";
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
    configInputs-stable = {
      type = "github";
      owner = "materusPL";
      repo = "nixos-config";
      ref = "inputs";
      inputs = {
        nixpkgs.follows = "nixpkgs-stable";
        home-manager.url = "github:nix-community/home-manager/release-23.05";
      };
    };
  };


  outputs = inputs @ { self, nixpkgs, home-manager, nur, ... }:
    let
      systems = [
        "x86_64-linux"
        "i686-linux"
        "aarch64-linux"
      ];
      forAllSystems = f: nixpkgs.lib.genAttrs systems (system: f system);
    in
    rec {
      nixosConfigurations = import ./configurations/host { inherit inputs; materusFlake = self; };
      homeConfigurations = import ./configurations/home { inherit inputs; materusFlake = self; };
      selfPath = ./.;

    };
}
