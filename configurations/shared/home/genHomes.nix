{ inputs, materusFlake, ... }:
let
  profiles = import (materusFlake.selfPath + "/configurations/profile");

  hosts = builtins.attrNames materusFlake.nixosConfigurations;
  genHomes = username:
    let
      #Make host specific user profile "username@host"
      _list = builtins.map (host: username + "@" + host) hosts;
      _for = i: (
        let len = builtins.length hosts; in
        ([{
          name = builtins.elemAt _list i;
          value = let host = builtins.elemAt hosts i; in
            materusFlake.nixosConfigurations.${host}.materusCfg.hm.lib.homeManagerConfiguration {
              pkgs = materusFlake.nixosConfigurations.${host}.pkgs;
              extraSpecialArgs = { materusCfg = materusFlake.nixosConfigurations.${host}.materusCfg // { isHm = true; }; };
              modules = [
                (materusFlake.selfPath + "/configurations/shared/home/${username}")
                (materusFlake.selfPath + "/configurations/host/${host}/home/${username}")
                profiles.homeProfile
                materusFlake.nixosConfigurations.${host}.materusCfg.configInputs.sops-nix.homeManagerModules.sops
              ];
            };
        }]
        ++ (if ((i + 1) < len) then _for (i + 1) else [ ]))
      );
    in
    (builtins.listToAttrs (_for 0)) // {
      #Make generic x86_64-linux user profile "username"
      ${username} =
        let
          materusCfg = {
            stable = false;
            inherit materusFlake;
            host = "Generic";
            hm = inputs.configInputs.home-manager;
            hmAsModule = false;
            nixerus = inputs.configInputs.nixerus;
            configInputs = inputs.configInputs;
            path = materusFlake.selfPath;
            isHm = true;
          };
        in
        inputs.configInputs.home-manager.lib.homeManagerConfiguration {
          pkgs = import inputs.nixpkgs { system = "x86_64-linux"; config = { allowUnfree = true; }; };
          extraSpecialArgs = { inherit materusCfg; };
          modules = [
            ./${username}
            profiles.homeProfile
            materusCfg.configInputs.sops-nix.homeManagerModules.sops
            materusCfg.configInputs.plasma-manager.homeManagerModules.plasma-manager
          ];
        };
    };
in
genHomes
