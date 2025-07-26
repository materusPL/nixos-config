{ config, pkgs, lib, materusArg, ... }:
{
  options.waffentragerService.scrobbling.enable = materusArg.pkgs.lib.mkBoolOpt false "Enable scrobbling";




  config =
    let
      cfg = config.waffentragerService.scrobbling;
    in


    #### MALOJA --------------------------------------------------------------------
    lib.mkIf cfg.enable {
      sops.templates."maloja.env".content = ''
        MALOJA_DATA_DIRECTORY=/data
        MALOJA_DIRECTORY_STATE=/data/state
        MALOJA_DIRECTORY_CACHE=/data/cache
        
        MALOJA_SKIP_SETUP=yes
        MALOJA_FORCE_PASSWORD=${config.sops.placeholder.maloja}
        MALOJA_SPOTIFY_API_ID=${config.sops.placeholder.spotify-client-id}
        MALOJA_SPOTIFY_API_SECRET=${config.sops.placeholder.spotify-client-secret}
        
        MALOJA_NAME=Melody

        MALOJA_WEEK_OFFSET=1

        PUID=${builtins.toString config.users.users.scrobbler.uid}
        PGID=${builtins.toString config.users.groups.scrobbler.gid}
        TC=Europe/Warsaw
        TIMEZONE=Europe/Warsaw
      '';
      services.nginx.virtualHosts = {
        "melody.materus.pl" = {
          sslTrustedCertificate = "/var/lib/mnt_acme/materus.pl/chain.pem";
          sslCertificateKey = "/var/lib/mnt_acme/materus.pl/key.pem";
          sslCertificate = "/var/lib/mnt_acme/materus.pl/fullchain.pem";
          addSSL = true;
          http2 = false;
          http3 = true;
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

        };

      };


      virtualisation.oci-containers.containers.maloja =
        {

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
      systemd.services."${config.virtualisation.oci-containers.backend}-maloja" =

        let
          malojaCfg = pkgs.writeText "settings.ini" ''[MALOJA]
directory_config = /data
lastfm_api_key = False
audiodb_api_key = False
spotify_api_id = False
spotify_api_secret = False
delimiters_feat = ["ft.","ft","feat.","feat","featuring","Ft.","Ft","Feat.","Feat","Featuring"]
delimiters_informal = ["vs.","vs","&","with"]
delimiters_formal = ["; ",";"]
metadata_providers = ["spotify","deezer","lastfm","audiodb","musicbrainz"]

'';
        in
        {
          requires = [ "elements-mount.service" ];
          after = [ "elements-mount.service" ];
          preStart = ''cp --update=none ${malojaCfg} ${config.waffentragerService.elements.malojaDir}/settings.ini'';

        };

      #### MULTI SCROBBLER --------------------------------------------------------------------
      users.groups.scrobbler = { gid = 3000; };
      users.users.scrobbler = {
        group = "scrobbler";
        uid = 3000;
        isSystemUser = true;
      };
      sops.templates."multi-scrobbler.env".content = ''
        TC=Europe/Warsaw
        CONFIG_DIR=/config
        PUID=${builtins.toString config.users.users.scrobbler.uid}
        PGID=${builtins.toString config.users.groups.scrobbler.gid}
      '';
      sops.templates."multi-scrobbler.json".owner = "scrobbler";
      sops.templates."multi-scrobbler.json".group = "scrobbler";
      sops.templates."multi-scrobbler.json".content = builtins.toJSON {
        baseUrl = "https://scrobbler.materus.pl";
        disableWeb = false;
        debugMode = false;
        sources = [
          {
            name = "materus-spotify";
            enable = true;
            clients = [ "maloja" ];
            data = {
              clientId = "${config.sops.placeholder.spotify-client-id}";
              clientSecret = "${config.sops.placeholder.spotify-client-secret}";
              redirectUri = "https://scrobbler.materus.pl/callback";
              interval = 30;
            };
            type = "spotify";
          }
          {
            name = "materus-jellyfin";
            enable = true;
            clients = [ "maloja" ];
            data = {
              users = [
                "materus"
              ];
              servers = [
                "waffentrager"
              ];
            };
            options = {
              logPayload = false;
              logFilterFailure = "warn";
            };
            type = "jellyfin";
          }
        ];
        clients = [
          {
            name = "maloja";
            enable = true;
            data = {
              url = "https://melody.materus.pl/";
              apiKey = "${config.sops.placeholder.maloja-api}";
            };
            type = "maloja";
          }
          {
            name = "materus-brainz";
            enable = true;
            configureAs = "client";
            data = {
              token = "${config.sops.placeholder.listenbrainz-api}";
              username = "materus";
            };
            type = "listenbrainz";
          }
          {
            name = "materus-lastfm";
            enable = true;
            configureAs = "client";
            data = {
              apiKey = "${config.sops.placeholder.lastfm-api}";
              secret = "${config.sops.placeholder.lastfm-secret}";
              redirectUri = "https://scrobbler.materus.pl/lastfm/callback";
            };
            type = "lastfm";
          }

        ];

      };




      services.nginx.virtualHosts = {
        "scrobbler.materus.pl" = {
          sslTrustedCertificate = "/var/lib/mnt_acme/materus.pl/chain.pem";
          sslCertificateKey = "/var/lib/mnt_acme/materus.pl/key.pem";
          sslCertificate = "/var/lib/mnt_acme/materus.pl/fullchain.pem";
          addSSL = true;
          http2 = false;
          http3 = true;
          locations."/" = {
            proxyPass = "http://127.0.0.1:42011";
            extraConfig = ''
              allow ${materusArg.ip-masks.wireguard.main};
              allow 192.168.100.0/24;
              deny all;
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
      systemd.services."${config.virtualisation.oci-containers.backend}-multi-scrobbler" =
        {
          preStart = ''cp -f ${config.sops.templates."multi-scrobbler.json".path} ${config.waffentragerService.elements.malojaDir}/multi-scrobbler/config.json'';
          requires = [ "elements-mount.service" ];
          after = [ "elements-mount.service" ];
        };
      virtualisation.oci-containers.containers.multi-scrobbler = {
        image = "foxxmd/multi-scrobbler:latest";
        ports = [
          "127.0.0.1:42011:9078"
        ];
        volumes = [
          "${config.waffentragerService.elements.malojaDir}/multi-scrobbler:/config"
        ];
        environmentFiles = [
          config.sops.templates."multi-scrobbler.env".path
        ];
      };






    };
}
