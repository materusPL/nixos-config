# * default.nix
{ inputs, configRootPath }:
let
  stable = inputs.config-stable;
  unstable = inputs.config-unstable;
  nixpkgs = stable.nixpkgs;
  nixpkgs-unstable = unstable.nixpkgs;
  mkkArg = {
    inherit stable;
    inherit unstable;
    inherit configRootPath;
  };
in
{
# * NixOS configurations
  nixosConfigurations = {
# ** materusPC

    "materusPC" = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      specialArgs = { mkkArg =  mkkArg // {current = stable;}; };
      modules = [
        ./common.nix
        ./hosts/materusPC.nix
      ];
    };

  };

}
