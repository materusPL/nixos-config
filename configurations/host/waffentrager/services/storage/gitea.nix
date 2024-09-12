{ materusArg, config, lib, ... }:
{
  options.waffentragerService.gitea.enable = materusArg.pkgs.lib.mkBoolOpt false "Enable gitea";


  config =
    let
      cfg = config.waffentragerService.gitea;
    in
    lib.mkMerge
      [
        (lib.mkIf cfg.enable {
          waffentragerService.postgresql.enable = true;
          waffentragerService.elements.enable = true;

          services.gitea.enable = true;
          services.gitea.lfs.enable = true;
          services.gitea.stateDir = "${config.waffentragerService.elements.path}/services/gitea";
          services.gitea.settings.service.DISABLE_REGISTRATION = true;
          services.gitea.settings.server.DOMAIN = "baka.materus.pl";
          services.gitea.settings.server.ROOT_URL = lib.mkForce "https://baka.materus.pl/";
          services.gitea.settings.server.PROTOCOL = "fcgi+unix";
          services.gitea.settings.cors = {
            ENABLED = true;
            X_FRAME_OPTIONS = "ALLOW-FROM https://*.materus.pl/";
          };

          services.gitea.database.type = "postgres";
          services.gitea.database.socket = "/var/run/postgresql/";

        })
        (lib.mkIf (cfg.enable && config.waffentragerService.nginx.enable) {

          services.nginx.virtualHosts = {
            "baka.materus.pl" = {
              sslTrustedCertificate = "/var/lib/mnt_acme/materus.pl/chain.pem";
              sslCertificateKey = "/var/lib/mnt_acme/materus.pl/key.pem";
              sslCertificate = "/var/lib/mnt_acme/materus.pl/fullchain.pem";
              addSSL = true;
              http2 = false;
              locations."/" = {
                extraConfig = ''
                  client_max_body_size 2G;
                  include ${config.services.nginx.package}/conf/fastcgi.conf;
                  include ${config.services.nginx.package}/conf/fastcgi_params;
                  proxy_http_version 1.1;
                  proxy_set_header    Host                $host;
                  proxy_set_header    X-Real-IP           $remote_addr;
                  proxy_set_header    X-Forwarded-Ssl     on;
                  proxy_set_header    X-Forwarded-For     $proxy_add_x_forwarded_for;
                  proxy_set_header    X-Forwarded-Proto   $scheme;

                  fastcgi_pass  unix:/var/run/gitea/gitea.sock;
                '';
              };

            };
          };
        }
        )
      ];
}
