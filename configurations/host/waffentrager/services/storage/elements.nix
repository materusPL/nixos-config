{ materusArg, config, lib, pkgs, ... }:
{
  options.waffentragerService.elements.enable = materusArg.pkgs.lib.mkBoolOpt false "Enable elements drive";
  options.waffentragerService.elements.path = lib.mkOption { default = "/var/lib/elements"; };
  options.waffentragerService.elements.uuid = lib.mkOption { default = "e32039c6-e98d-44b0-8e7d-120994bf7be1"; };
  options.waffentragerService.elements.postgresqlDir = lib.mkOption { default = "${config.waffentragerService.elements.path}/services/postgresql"; };
  options.waffentragerService.elements.nextcloudDir = lib.mkOption { default = "${config.waffentragerService.elements.path}/services/nextcloud"; };
  options.waffentragerService.elements.lldapDir = lib.mkOption { default = "${config.waffentragerService.elements.path}/services/lldap"; };
  options.waffentragerService.elements.jellyfinDir = lib.mkOption { default = "${config.waffentragerService.elements.path}/services/jellyfin"; };
  options.waffentragerService.elements.malojaDir = lib.mkOption { default = "${config.waffentragerService.elements.path}/services/maloja"; };

  config =
    let
      cfg = config.waffentragerService.elements;
    in
    lib.mkIf cfg.enable {

      systemd.services.elements-mount = {
        description = "Decrypt and mount elements drive";
        wantedBy = [ "multi-user.target" ];
        path = [ pkgs.cryptsetup pkgs.coreutils pkgs.util-linux ];
        serviceConfig.Type = "oneshot";
        serviceConfig.RemainAfterExit = true;
        script = ''
          mkdir -p ${cfg.path}
          cryptsetup luksOpen /dev/disk/by-uuid/${cfg.uuid} elements -d ${config.sops.secrets.elements.path}
          mount /dev/mapper/elements ${cfg.path}
        ''

        ;
        preStop = ''
          umount ${cfg.path}
          cryptsetup luksClose elements
        '';
      };

      systemd.services.elements-dirmake = {
        description = "Create dirs in elements drive";
        path = [ pkgs.cryptsetup pkgs.coreutils pkgs.util-linux ];

        serviceConfig.Type = "oneshot";
        serviceConfig.RemainAfterExit = false;
        script = lib.optionalString config.waffentragerService.postgresql.enable ''
          mkdir -p ${cfg.postgresqlDir}/${config.waffentragerService.postgresql.version}
          chown -R postgres:postgres ${cfg.postgresqlDir}
        '' + lib.optionalString config.waffentragerService.nextcloud.enable ''
          mkdir -p ${cfg.nextcloudDir}
          chown -R nextcloud:nextcloud ${cfg.nextcloudDir}
        '' + lib.optionalString config.waffentragerService.auth.lldap.enable ''
          mkdir -p ${cfg.lldapDir}
          chown -R lldap:lldap ${cfg.lldapDir}
        '' + lib.optionalString config.waffentragerService.jellyfin.enable ''
          mkdir -p ${cfg.jellyfinDir}
          chown -R materus:nextcloud ${cfg.jellyfinDir}
        '' + lib.optionalString config.waffentragerService.scrobbling.enable ''
          mkdir -p ${cfg.malojaDir}/multi-scrobbler
          chown -R scrobbler:scrobbler ${cfg.malojaDir}
        ''


        ;
      };

    };
}
