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
        requires = [ "elements-mount.service" ];
        after = [ "elements-mount.service" ];
      };
      systemd.services.samba-wsdd = {
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
	        display charset = UTF-8
          catia:mappings = 0x22:0xa8,0x2a:0xa4,0x2f:0xf8,0x3a:0xf7,0x3c:0xab,0x3e:0xbb,0x3f:0xbf,0x5c:0xff,0x7c:0xa6
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
