{ lib, pkgs, materusArg, config, ... }:
{
  options.waffentragerService.samba.enable = materusArg.pkgs.lib.mkBoolOpt false "Enable samba";

  config =
    let
      cfg = config.waffentragerService.samba;
    in
    lib.mkIf cfg.enable {
      services.samba-wsdd.enable = true;
      services.samba-wsdd.openFirewall = true;
      services.samba = {
        enable = true;
        package = pkgs.sambaFull;
        securityType = "user";
        openFirewall = true;
        extraConfig = ''
          workgroup = WORKGROUP
          server string = smbwaffentrager
          netbios name = smbwaffentrager
          security = user 
          hosts allow = ${materusArg.wireguard.sambaIp} 192.168.100. 127.0.0.1 localhost
          hosts deny = 0.0.0.0/0
          guest account = nobody
          map to guest = bad user
        '';
        shares = {
          materus = {
            path = "${config.waffentragerService.elements.path}/storage/materus";
            browseable = "yes";
            "read only" = "no";
            "guest ok" = "no";
            "create mask" = "0644";
            "directory mask" = "0755";
            "force user" = "materus";
            "force group" = "users";
          };
        };
      };
    };
}
