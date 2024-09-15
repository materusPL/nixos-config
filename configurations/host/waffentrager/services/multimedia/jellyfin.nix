{ lib, config, materusArg, ... }:
{
  options.waffentragerService.jellyfin.enable = materusArg.pkgs.lib.mkBoolOpt false "Enable jellyfin";

  config =
    let
      cfg = config.waffentragerService.jellyfin;
    in
    lib.mkIf cfg.enable {
      services.jellyfin = rec {
        enable = true;
        openFirewall = true;
        user = "materus";
        group = "nextcloud";
        dataDir = config.waffentragerService.elements.jellyfinDir;
        cacheDir = "${dataDir}/cache";
      };
      /*
      services.jellyseerr = {
        enable = true;
        openFirewall = true;
      };*/

      services.nginx = {
        appendHttpConfig = ''
          map $request_uri $h264Level { ~(h264-level=)(.+?)& $2; }
          map $request_uri $h264Profile { ~(h264-profile=)(.+?)& $2; }
        '';
        proxyCachePath."jellyfin" = {
          enable = true;
          maxSize = "1g";
          levels = "1:2";
          keysZoneName = "jellyfin";
          keysZoneSize = "100m";
          inactive = "1d";
          useTempPath = false;

        };
        virtualHosts = {
          "noot.materus.pl" = {
            extraConfig = ''
              client_max_body_size 20M;
              add_header X-Frame-Options "SAMEORIGIN";
              add_header X-XSS-Protection "0"; # Do NOT enable. This is obsolete/dangerous
              add_header X-Content-Type-Options "nosniff";
              add_header Permissions-Policy "accelerometer=(), ambient-light-sensor=(), battery=(), bluetooth=(), camera=(), clipboard-read=(), display-capture=(), document-domain=(), encrypted-media=(), gamepad=(), geolocation=(), gyroscope=(), hid=(), idle-detection=(), interest-cohort=(), keyboard-map=(), local-fonts=(), magnetometer=(), microphone=(), payment=(), publickey-credentials-get=(), serial=(), sync-xhr=(), usb=(), xr-spatial-tracking=()" always;
            '';
            sslTrustedCertificate = "/var/lib/mnt_acme/materus.pl/chain.pem";
            sslCertificateKey = "/var/lib/mnt_acme/materus.pl/key.pem";
            sslCertificate = "/var/lib/mnt_acme/materus.pl/fullchain.pem";
            addSSL = true;
            http2 = false;
            http3 = true;
            locations."~ /Items/(.*)/Images" = {
              proxyPass = "http://127.0.0.1:8096";
              extraConfig = ''
                proxy_set_header Host $host;
                proxy_set_header X-Real-IP $remote_addr;
                proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
                proxy_set_header X-Forwarded-Proto $scheme;
                proxy_set_header X-Forwarded-Protocol $scheme;
                proxy_set_header X-Forwarded-Host $http_host;

                proxy_cache jellyfin;
                proxy_cache_revalidate on;
                proxy_cache_lock on;
              '';
            };
            locations."~ ^/web/htmlVideoPlayer-plugin.[0-9a-z]+.chunk.js$" = {
              proxyPass = "http://127.0.0.1:8096";
              extraConfig = ''
                proxy_set_header Accept-Encoding "";

                sub_filter_types *;
                sub_filter 'return u=30' 'return u=600';
                sub_filter 'return u=6' 'return u=60';
                sub_filter 'maxBufferLength:u' 'maxBufferLength:u,maxBufferSize:180000000';
                sub_filter_once on;
              '';
            };
            locations."~* ^/Videos/(.*)/(?!live)" = {
              proxyPass = "http://127.0.0.1:8096";
              extraConfig = ''
                # Set size of a slice (this amount will be always requested from the backend by nginx)
                # Higher value means more latency, lower more overhead
                # This size is independent of the size clients/browsers can request
                slice 2m;

                proxy_cache jellyfin;
                proxy_cache_valid 200 206 301 302 30d;
                proxy_ignore_headers Expires Cache-Control Set-Cookie X-Accel-Expires;
                proxy_cache_use_stale error timeout invalid_header updating http_500 http_502 http_503 http_504;
                proxy_connect_timeout 15s;
                proxy_http_version 1.1;
                proxy_set_header Connection "";
                # Transmit slice range to the backend
                proxy_set_header Range $slice_range;

                # This saves bandwidth between the proxy and jellyfin, as a file is only downloaded one time instead of multiple times when multiple clients want to at the same time
                # The first client will trigger the download, the other clients will have to wait until the slice is cached
                # Esp. practical during SyncPlay
                proxy_cache_lock on;
                proxy_cache_lock_age 60s;

                proxy_cache_key "jellyvideo$uri?MediaSourceId=$arg_MediaSourceId&VideoCodec=$arg_VideoCodec&AudioCodec=$arg_AudioCodec&AudioStreamIndex=$arg_AudioStreamIndex&VideoBitrate=$arg_VideoBitrate&AudioBitrate=$arg_AudioBitrate&SubtitleMethod=$arg_SubtitleMethod&TranscodingMaxAudioChannels=$arg_TranscodingMaxAudioChannels&RequireAvc=$arg_RequireAvc&SegmentContainer=$arg_SegmentContainer&MinSegments=$arg_MinSegments&BreakOnNonKeyFrames=$arg_BreakOnNonKeyFrames&h264-profile=$h264Profile&h264-level=$h264Level&slicerange=$slice_range";

              '';
            };
            locations."/" = {
              proxyPass = "http://127.0.0.1:8096";
              extraConfig = ''
                proxy_pass_request_headers on;

                proxy_set_header Host $host;

                proxy_set_header X-Real-IP $remote_addr;
                proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
                proxy_set_header X-Forwarded-Proto $scheme;
                proxy_set_header X-Forwarded-Host $http_host;

                proxy_set_header Upgrade $http_upgrade;
                proxy_set_header Connection $http_connection;
                

              '';
            };
            locations."/socket" = {
              proxyPass = "http://127.0.0.1:8096";
              extraConfig = ''
                proxy_pass_request_headers on;

                proxy_http_version 1.1;
                proxy_set_header Upgrade $http_upgrade;
                proxy_set_header Connection "upgrade";
                proxy_set_header Host $host;
                proxy_set_header X-Real-IP $remote_addr;
                proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
                proxy_set_header X-Forwarded-Proto $scheme;
                proxy_set_header X-Forwarded-Protocol $scheme;
                proxy_set_header X-Forwarded-Host $http_host;


              '';
            };

          };
        };
      };
    };
}
