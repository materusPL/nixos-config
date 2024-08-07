{ lib, pkgs, materusArg, config, ... }:
{
  options.waffentragerService.samba.enable = materusArg.pkgs.lib.mkBoolOpt false "Enable samba";

  config =
    let
      cfg = config.waffentragerService.samba;
    in
    lib.mkIf cfg.enable {
      waffentragerService.elements.enable = true;

      systemd.services.samba-nmbd = {
        partOf = [ "elements-mount.service" ];
        requires = [ "elements-mount.service" ];
        after = [ "elements-mount.service" ];
      };
      systemd.services.samba-wsdd = {
        partOf = [ "elements-mount.service" ];
        requires = [ "elements-mount.service" ];
        after = [ "elements-mount.service" ];
      };
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
          mangled names = no
          dos charset = CP850
          unix charset = UTF-8
        '';
        shares = {
          materus = {
            path = "${config.waffentragerService.elements.path}/storage/materus";
            browseable = "yes";
            "read only" = "no";
            "guest ok" = "no";
            "create mask" = "0770";
            "directory mask" = "0770";
            "force user" = "materus";
            "force group" = "nextcloud";
          };
        };
      };
    };
}
