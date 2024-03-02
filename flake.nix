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
      ref = "nixos-23.11";
    };

    hm-stable = {
      type = "github";
      owner = "nix-community";
      repo = "home-manager";
      ref = "release-23.11";
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


  outputs = inputs @ { self, nixpkgs, home-manager, nur, ... }:
    rec {
      nixosConfigurations = import ./configurations/host { inherit inputs; materusFlake = self; };
      homeConfigurations = import ./configurations/home { inherit inputs; materusFlake = self; };
      selfPath = self;
      decrypted = builtins.pathExists (selfPath + "/decrypted");
    };
}
