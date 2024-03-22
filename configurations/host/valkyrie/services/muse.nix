{ config, pkgs, lib, materusArg, ... }:
{
  options.valkyrieService.muse.enable = materusArg.pkgs.lib.mkBoolOpt false "Enable pihole";




  config =
    let
      cfg = config.valkyrieService.muse;
    in
    lib.mkIf cfg.enable {
      sops.templates."muse.env".content = ''
        CACHE_LIMIT=512MB
        BOT_STATUS=online
        BOT_ACTIVITY_TYPE=LISTENING
        BOT_ACTIVITY=Coś
        DISCORD_TOKEN=${config.sops.placeholder.discord-token}
        YOUTUBE_API_KEY=${config.sops.placeholder.youtube-api}
        SPOTIFY_CLIENT_ID=${config.sops.placeholder.spotify-client-id}
        SPOTIFY_CLIENT_SECRET=${config.sops.placeholder.spotify-client-secret}
      '';

      systemd.tmpfiles.rules = [
        "d    /var/lib/muse  0776    root    root     -"
      ];

      virtualisation.oci-containers.containers.muse = {
        image = "codetheweb/muse:latest";
        volumes = [
          "/var/lib/muse:/data"
        ];
        environmentFiles = [
          config.sops.templates."muse.env".path
        ];
      };

    };


}
