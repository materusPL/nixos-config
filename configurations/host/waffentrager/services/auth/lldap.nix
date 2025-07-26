{ config, pkgs, lib, materusArg, ... }:
{
  options.waffentragerService.auth.lldap.enable = materusArg.pkgs.lib.mkBoolOpt false "Enable lldap";
  config =
    let
      cfg = config.waffentragerService.auth.lldap;
    in
    lib.mkIf cfg.enable {
      waffentragerService.elements.enable = true;
      waffentragerService.nginx.enable = true;
      services.nginx.virtualHosts."mamba.podkos.pl" = {
        forceSSL = true;
        http3 = true;
        sslTrustedCertificate = "/var/lib/mnt_acme/mamba.podkos.pl/chain.pem";
        sslCertificateKey = "/var/lib/mnt_acme/mamba.podkos.pl/key.pem";
        sslCertificate = "/var/lib/mnt_acme/mamba.podkos.pl/fullchain.pem";
        locations."/" = {
          proxyPass = "http://127.0.0.1:17170";
          extraConfig = ''
            proxy_set_header    Host                $host;
            proxy_set_header    X-Real-IP           $remote_addr;
            proxy_set_header    X-Forwarded-Ssl     on;
            proxy_set_header    X-Forwarded-For     $proxy_add_x_forwarded_for;
            proxy_set_header    X-Forwarded-Proto   $scheme;


            allow ${materusArg.ip-masks.wireguard.main};
            allow 192.168.100.0/24;
            deny all;
          '';
        };
      };

      systemd.services.lldap = {
        requires = [ "elements-mount.service" ];
        after = [ "elements-mount.service" ];
        serviceConfig = {
          DynamicUser = lib.mkForce false;
          WorkingDirectory = lib.mkForce config.waffentragerService.elements.lldapDir;
        };
      };
      users.groups.lldap = { };
      users.users.lldap = {
        group = "lldap";
        isSystemUser = true;
      };
      sops.secrets.jwt = { owner = "lldap"; group = "lldap"; mode = "0440"; };
      sops.secrets."lldap-database" = { owner = "lldap"; group = "lldap"; };
      services.lldap.enable = true;
      services.lldap.environmentFile = config.sops.templates."lldap.env".path;
      sops.templates."lldap.env" = {
        content = ''
          LLDAP_JWT_SECRET_FILE="${config.sops.secrets.jwt.path}"
          LLDAP_DATABASE_URL="postgres://lldap:${config.sops.placeholder."lldap-database"}@%2Fvar%2Frun%2Fpostgresql/lldap"
        '';
        owner = "lldap";
        group = "lldap";
      };

      services.lldap.settings = {
        ldap_base_dn = "dc=podkos,dc=pl";

        ldap_host = "127.0.0.1";
        http_url = "https://mamba.podkos.pl";
        ldap_user_dn = "master";
        ldap_user_email = "materus@podkos.pl";
        ldap_port = 3890;
        key_seed = materusArg.waffentrager.lldap.seed;
      };
    };
}
