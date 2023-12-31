{ inputs, materusFlake, ... }:
let
  profiles = import ../profile;

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
              extraSpecialArgs = { materusCfg = materusFlake.nixosConfigurations.${host}.materusCfg; };
              modules = [
                ./${username}
                ../host/${host}/extraHome.nix
                profiles.homeProfile
                inputs.private.homeModule
              ];
            };
        }]
        ++ (if ((i + 1) < len) then _for (i + 1) else [ ]))
      );
    in
    (builtins.listToAttrs (_for 0)) // {
      #Make generic x86_64-linux user profile "username"
      ${username} =
        let materusCfg = {
          stable = false;
          inherit materusFlake;
          host = "Generic";
          hm = inputs.configInputs.inputs.home-manager;
          nixerus = inputs.configInputs.inputs.nixerus;
          configInputs = inputs.configInputs;
          path = materusFlake.selfPath;
        }; in
        inputs.configInputs.inputs.home-manager.lib.homeManagerConfiguration {
          pkgs = import inputs.nixpkgs { system = "x86_64-linux"; config = { allowUnfree = true; }; };
          extraSpecialArgs = { inherit materusCfg; };
          modules = [
            ./${username}
            profiles.homeProfile
            inputs.private.homeModule
          ];
        };
    };
in
genHomes
