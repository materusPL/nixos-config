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
  nixosConfigurations =
    let
      mkSystem =
        hostname:
        {
          system ? "x86_64-linux",
          isStable ? true,
          extraArgs ? { },
          extraModules ? [ ],
        }:
        (if isStable then nixpkgs else nixpkgs-unstable).lib.nixosSystem {
          system = system;
          specialArgs = {
            mkkArg =
              mkkArg
              // {
                current = (if isStable then stable else unstable);
                isDecrypted = (isDecrypted (if isStable then stable else unstable).nixpkgs system);
                isStable = isStable;
                isOs = true;
              }
              // extraArgs;
          };
          modules = [
            ./common.nix
            ./hosts/${hostname}.nix
            (
              if
                (
                  (isDecrypted (if isStable then stable else unstable).nixpkgs system)
                  && builtins.pathExists ./hosts/${hostname}-private.nix
                )
              then
                ./hosts/${hostname}-private.nix
              else
                { }
            )
            
          ] ++ extraModules;
        };

    in
    {
# ** materusPC
      "materusPC" = mkSystem "materusPC" { };
# * default.nix END
    };

}
