{ config, pkgs, lib, materusArg, ... }:
{
  options.waffentragerService.scrobbling.enable = materusArg.pkgs.lib.mkBoolOpt false "Enable scrobbling";




  config =
    let
      cfg = config.waffentragerService.scrobbling;
    in


    #### MALOJA
    lib.mkIf cfg.enable {
      sops.templates."maloja.env".content = ''
        MALOJA_DATA_DIRECTORY=/data
        MALOJA_DIRECTORY_CONFIG=/data/config
        MALOJA_DIRECTORY_STATE=/data/state
        MALOJA_DIRECTORY_CACHE=/data/cache
        MALOJA_NAME="Melody"
        MALOJA_LASTFM_USERNAME=${config.sops.placeholder.lastfm-user}
        MALOJA_LASTFM_PASSWORD=${config.sops.placeholder.lastfm-pass}
        MALOJA_LASTFM_API_KEY=${config.sops.placeholder.lastfm-api}
        MALOJA_LASTFM_API_SECRET=${config.sops.placeholder.lastfm-secret}
        MALOJA_LASTFM_API_SK=${config.sops.placeholder.lastfm-token}
        MALOJA_SKIP_SETUP=yes
        MALOJA_FORCE_PASSWORD=${config.sops.placeholder.maloja}
        MALOJA_SPOTIFY_API_ID=${config.sops.placeholder.spotify-client-id}
        MALOJA_SPOTIFY_API_SECRET=${config.sops.placeholder.spotify-client-secret}
      '';

      virtualisation.oci-containers.containers.maloja = {
        image = "krateng/maloja:latest";
        ports = [
          "42010:42010"
        ];
        volumes = [
          "${config.waffentragerService.elements.malojaDir}:/data"
        ];
        environmentFiles = [
          config.sops.templates."maloja.env".path
        ];
      };
      systemd.services."${config.virtualisation.oci-containers.backend}-maloja" = {
        requires = [ "elements-mount.service" ];
        after = [ "elements-mount.service" ];
      };

      #### MULTI SCROBBLER
      sops.templates."multi-scrobbler.env".content = ''
          BASE_URL="https://melody.materus.pl/multi-scrobbler"
          TC=Europe/Warsaw

          JELLYFIN_SERVER="https://noot.materus.pl/"
          SPOTIFY_CLIENT_ID=${config.sops.placeholder.spotify-client-id}
          SPOTIFY_CLIENT_SECRET=${config.sops.placeholder.spotify-client-secret}
          MALOJA_URL="https://melody.materus.pl"
          MALOJA_API_KEY="${config.sops.placeholder.maloja-api}"
          LASTFM_API_KEY=${config.sops.placeholder.lastfm-api}
          LASTFM_SECRET=${config.sops.placeholder.lastfm-secret}
      '';
      virtualisation.oci-containers.containers.multi-scrobbler = {
        image = "foxxmd/multi-scrobbler:latest";
        ports = [
          "42011:9078"
        ];
        volumes = [
          "${config.waffentragerService.elements.malojaDir}/multi-scrobbler:/data"
        ];
        environmentFiles = [
          config.sops.templates."multi-scrobbler.env".path
        ];
      };


      
      #### Proxy
      services.nginx.virtualHosts = {
        "melody.materus.pl" = {
          sslTrustedCertificate = "/var/lib/mnt_acme/materus.pl/chain.pem";
          sslCertificateKey = "/var/lib/mnt_acme/materus.pl/key.pem";
          sslCertificate = "/var/lib/mnt_acme/materus.pl/fullchain.pem";
          addSSL = true;
          http2 = false;
          http3 = true;
          # Maloja
          locations."/" = {
            proxyPass = "http://127.0.0.1:42010";
            extraConfig = ''
              proxy_http_version 1.1;
              proxy_set_header    Host                $host;
              proxy_set_header    X-Real-IP           $remote_addr;
              proxy_set_header    X-Forwarded-Ssl     on;
              proxy_set_header    X-Forwarded-For     $proxy_add_x_forwarded_for;
              proxy_set_header    X-Forwarded-Proto   $scheme;

            '';
          };

          locations."/multi-scrobbler" = {
            proxyPass = "http://127.0.0.1:42011";
            extraConfig = ''
              proxy_http_version 1.1;
              proxy_set_header    Host                $host;
              proxy_set_header    X-Real-IP           $remote_addr;
              proxy_set_header    X-Forwarded-Ssl     on;
              proxy_set_header    X-Forwarded-For     $proxy_add_x_forwarded_for;
              proxy_set_header    X-Forwarded-Proto   $scheme;

            '';
          };

        };

      };


    };
}
