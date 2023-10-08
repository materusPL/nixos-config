{
  description = "Materus hosts and user config";
  inputs = {
    nixpkgs = {
      type = "github";
      owner = "NixOS";
      repo = "nixpkgs";
      ref = "nixos-unstable";
    };
    private = {
      type = "github";
      owner = "materusPL";
      repo = "Nixerus";
      ref = "mock";
    };
    configInputs = {
      url = "git+file:/materus/config/nixos-config?ref=inputs";
      inputs = {
        nixpkgs.follows = "nixpkgs";
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
