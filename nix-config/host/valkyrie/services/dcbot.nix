{
  config,
  pkgs,
  lib,
  mkk,
  ...
}:
{
  options.valkyrieService.dcbot.enable = mkk.lib.mkBoolOpt false "Enable muse bot";

  config =
    let
      cfg = config.valkyrieService.dcbot;
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
      virtualisation.oci-containers.containers.dcbot = {
        image = "sl33ping/muse:pr-1195";
        volumes = [ "/var/lib/muse:/data" ];
        environmentFiles = [ config.sops.templates."muse.env".path ];
      };

    };

}
