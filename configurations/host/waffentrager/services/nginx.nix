{ materusArg, config, lib, pkgs, ... }:
{
  options.waffentragerService.nginx.enable = materusArg.pkgs.lib.mkBoolOpt false "Enable nginx";


  config =
    let
      cfg = config.waffentragerService.nginx;
    in
    lib.mkIf cfg.enable {
      networking.firewall.allowedTCPPorts = [ 80 443 ];
      services.nginx = {
        enable = true;
        recommendedTlsSettings = true;
        recommendedOptimisation = true;
        recommendedGzipSettings = true;
        package = pkgs.tengine;
        virtualHosts."default" = {
          sslTrustedCertificate = "/var/lib/mnt_acme/materus.pl/chain.pem";
          sslCertificateKey = "/var/lib/mnt_acme/materus.pl/key.pem";
          sslCertificate = "/var/lib/mnt_acme/materus.pl/fullchain.pem";
          forceSSL = true;
          http2 = false;
          default = true;
          locations."/" = { extraConfig = ''deny all;''; };
        };
      };

      systemd.services.nginx = {
        requires = [ "var-lib-mnt_acme.mount" ];
        after = [ "var-lib-mnt_acme.mount" ];
        serviceConfig = {
          restart = "always";
          restartSec = 60;
        };
      };
    };

}
