{ lib, config, materusArg, ... }:
{
  options.waffentragerService.jellyfin.enable = materusArg.pkgs.lib.mkBoolOpt false "Enable jellyfin";

  config =
    let
      cfg = config.waffentragerService.jellyfin;
    in
    lib.mkIf cfg.enable {
      services.jellyfin = {
        enable = true;
        openFirewall = true;
        user = "materus";
        group = "nextcloud";
        dataDir = config.waffentragerService.elements.jellyfinDir;
      };

      services.nginx.virtualHosts = {
            "noot.materus.pl" = {
              sslTrustedCertificate = "/var/lib/mnt_acme/materus.pl/chain.pem";
              sslCertificateKey = "/var/lib/mnt_acme/materus.pl/key.pem";
              sslCertificate = "/var/lib/mnt_acme/materus.pl/fullchain.pem";
              addSSL = true;
              http2 = false;
              http3 = true;
              locations."/" = {
                proxyPass = "http://127.0.0.1:8096";
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

                '';
              };

            };
          };
    };
}
