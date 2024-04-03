{ config, pkgs, lib, materusArg, ... }:
{
  options.valkyrieService.dcbot.enable = materusArg.pkgs.lib.mkBoolOpt false "Enable dcbot";




  config =
    let
      cfg = config.valkyrieService.dcbot;
    in
    lib.mkIf cfg.enable {
      sops.templates."dcbot.env".content = ''
          TOKEN=${config.sops.placeholder.discord-token}
          MAX_PLAYLIST_SIZE=10
          PRUNING=false
          LOCALE=pl
          DEFAULT_VOLUME=100
          STAY_TIME=30
      '';

      systemd.tmpfiles.rules = [
        "d    /var/lib/muse  0776    root    root     -"
      ];

      virtualisation.oci-containers.containers.dcbot = {
        image = "eritislami/evobot:latest";
        volumes = [
        ];
        environmentFiles = [
          config.sops.templates."dcbot.env".path
        ];
      };

    };


}
