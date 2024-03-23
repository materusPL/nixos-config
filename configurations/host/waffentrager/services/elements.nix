{ materusArg, config, lib, pkgs, ... }:
{
  options.waffentragerService.elements.enable = materusArg.pkgs.lib.mkBoolOpt false "Enable elements drive";
  options.waffentragerService.elements.path = lib.mkOption { default = "/var/lib/elements"; };
  options.waffentragerService.elements.uuid = lib.mkOption { default = "e32039c6-e98d-44b0-8e7d-120994bf7be1"; };

  config =
    let
      cfg = config.waffentragerService.elements;
    in
    lib.mkIf cfg.enable {

      systemd.services.elements-mount = {
        wantedBy = [ "multi-user.target" ];
        path = [ pkgs.cryptsetup pkgs.coreutils pkgs.util-linux ];
        serviceConfig.Type = "oneshot";
        serviceConfig.RemainAfterExit = true;
        script = ''
          mkdir -p ${cfg.path}
          cryptsetup luksOpen /dev/disk/by-uuid/${cfg.uuid} elements -d ${config.sops.secrets.elements.path}
          mount /dev/mapper/elements ${cfg.path}
        '';
        preStop = ''
          umount ${cfg.path}
          cryptsetup luksClose elements
        '';
      };

    };
}
