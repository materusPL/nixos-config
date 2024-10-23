{ inputs, materusFlake }:

let
  profiles = import ../profile;

  makeSystem = { host, arch ? "x86_64-linux", extraModules ? [ ], stable ? true, hmAsModule ? true, hmUsers ? [ "materus" ] }:
    let
      nixosSystem = if stable then inputs.nixpkgs-stable.lib.nixosSystem else inputs.nixpkgs.lib.nixosSystem;
      hm = if stable then inputs.configInputs-stable.home-manager else inputs.configInputs.home-manager;
      materusCfg = {
        inherit stable;
        inherit materusFlake;
        inherit host;
        inherit hm;
        inherit hmAsModule;
        inherit arch;
        nixerus = if stable then inputs.configInputs-stable.nixerus else inputs.configInputs.nixerus;
        configInputs = if stable then inputs.configInputs-stable else inputs.configInputs;
        path = materusFlake.selfPath;
        hostPath = materusFlake.selfPath + "/configurations/host/${host}";
        isHm = false;
      };
    in
    (nixosSystem {
      specialArgs = { inherit materusCfg; };
      system = arch;
      modules = [
        ./${host}
        profiles.osProfile
        materusCfg.configInputs.sops-nix.nixosModules.sops
        (if hmAsModule then hm.nixosModules.home-manager else { })
        (if hmAsModule then
          {

            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.sharedModules = [ materusCfg.configInputs.plasma-manager.homeManagerModules.plasma-manager ];
            home-manager.users = (builtins.foldl' (a: b: a // b) { } (builtins.map
              (user: {
                ${user} = ({ ... }:
                  {
                    imports = [
                      (materusFlake.selfPath + "/configurations/shared/home/${user}")
                      (materusFlake.selfPath + "/configurations/host/${host}/home/${user}")
                      profiles.homeProfile
                    ];
                  });
              })
              hmUsers));
            home-manager.extraSpecialArgs = { materusCfg = materusCfg // { isHm = true; }; };
          } else { })

      ] ++ extraModules;
    }) // { inherit materusCfg; };
in
{
  materusPC = makeSystem { host = "materusPC"; stable = true; };
  flamaster = makeSystem { host = "flamaster"; stable = true; };
  valkyrie = makeSystem { host = "valkyrie"; stable = true; };
  waffentrager = makeSystem { host = "waffentrager"; stable = true; arch = "aarch64-linux"; };

  Old-materusPC = makeSystem { host = "Old-materusPC"; stable = true; };
}
