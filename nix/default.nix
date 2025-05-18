# * Outputs - default.nix
{ inputs, configRootPath }:
let
  isDecrypted =
    npkgs: system:
    let
      file = ../private/check-encryption;
      resultFile = npkgs.legacyPackages."${system}".runCommandLocal "check-encryption" { src = file; } ''
        if [[ "$(< $src)" != "DECRYPTED" ]]; then
                 echo -n "no" >> $out;
              else
                 echo -n "yes" >> $out;
              fi 
      '';
    in
      (builtins.readFile resultFile == "yes");

  
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

    "materusPC" = nixpkgs.lib.nixosSystem rec {
      system = "x86_64-linux";
      specialArgs = { mkkArg =  mkkArg // {current = stable;}; };
      modules = [
        ./hosts/materusPC.nix
        (if (isDecrypted stable.nixpkgs system) then ./hosts/materusPC-private.nix else {} )
        ./common.nix
        
      ];
    };
# * default.nix END
  };

}
