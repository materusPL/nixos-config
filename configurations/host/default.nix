{ inputs, materusFlake }:

let
  profiles = import ../profile;

  makeSystem = { host, arch ? "x86_64-linux", extraModules ? [ ], stable ? true }:
    let
      nixosSystem = if stable then inputs.nixpkgs-stable.lib.nixosSystem else inputs.nixpkgs.lib.nixosSystem;
      hm = if stable then inputs.configInputs-stable.inputs.home-manager else inputs.configInputs.inputs.home-manager;
      materusCfg = {
        inherit stable;
        inherit materusFlake;
        inherit host;
        inherit hm;
        nixerus = if stable then inputs.configInputs-stable.inputs.nixerus else inputs.configInputs.inputs.nixerus;
        configInputs = if stable then inputs.configInputs-stable else inputs.configInputs;
        path = materusFlake.selfPath;
        isHm = false;
      };
    in
    (nixosSystem {
      specialArgs = { inherit materusCfg; };
      system = arch;
      modules = [
        ./${host}
        inputs.private.systemModule
        profiles.osProfile
      ] ++ extraModules;
    }) // { inherit materusCfg; };
in
{
  materusPC = makeSystem { host = "materusPC"; stable = false; };
  flamaster = makeSystem { host = "flamaster"; stable = true; };
  valkyrie = makeSystem { host = "valkyrie"; stable = true; };
  waffentrager = makeSystem { host = "waffentrager"; stable = false; arch = "aarch64-linux"; extraModules = [ ]; };

  Old-materusPC = makeSystem { host = "Old-materusPC"; stable = false; };
}
