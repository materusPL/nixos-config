{ config, pkgs, lib, materusArg, ... }:
{
  options.waffentragerService.auth.authelia.enable = materusArg.pkgs.lib.mkBoolOpt false "Enable authelia";
  config =
    let
      cfg = config.waffentragerService.auth.authelia;
      port = 9091;
    in
    lib.mkIf cfg.enable {
      sops.secrets."authelia-storagekey" = { owner = "authelia"; };
      sops.secrets."authelia-database" = { owner = "authelia"; };
      sops.secrets."ldap-master" = { owner = "authelia"; };
      users.users.authelia = {
        group = "lldap";
        isSystemUser = true;
      };
      services.authelia.instances.main = {
        enable = true;
        user = "authelia";
        environmentVariables = {
          AUTHELIA_AUTHENTICATION_BACKEND_LDAP_PASSWORD_FILE = config.sops.secrets."ldap-master".path;
          AUTHELIA_STORAGE_POSTGRES_PASSWORD_FILE = config.sops.secrets."authelia-database".path;
        };
        secrets = {
          jwtSecretFile = config.sops.secrets.jwt.path;
          storageEncryptionKeyFile = config.sops.secrets."authelia-storagekey".path;
        };
        settings = {
          access_control = {
            default_policy = "one_factor";
          };
          authentication_backend = {
            ldap.url = "ldap://127.0.0.1:3890";
            ldap.implementation = "custom";
            ldap.base_dn = config.services.lldap.settings.ldap_base_dn;
            ldap.user = "CN=master,ou=people,DC=podkos,DC=pl";
            ldap.additional_users_dn = "OU=people";
            ldap.users_filter = "(&({username_attribute}={input})(objectClass=person))";
            ldap.additional_groups_dn = "OU=groups";
            ldap.groups_filter = "(&(member={dn})(objectClass=groupOfNames))";
          };
          storage = {
            postgres.host = "/var/run/postgresql";
            postgres.port = "5432";
            postgres.database = "authelia";
            postgres.username = "authelia";

          };
          notifier = {
            disable_startup_check = false;
            filesystem.filename = "/tmp/test_notification.txt";
          };
          session = {
            name = "materus-session";
            domain = "materus.pl";
          };

          default_redirection_url = "https://materus.pl";
          server.port = port;
        };
      };
      services.nginx.virtualHosts."gatekeeper.materus.pl" = {
        forceSSL = true;
        http3 = true;
        sslTrustedCertificate = "/var/lib/mnt_acme/materus.pl/chain.pem";
        sslCertificateKey = "/var/lib/mnt_acme/materus.pl/key.pem";
        sslCertificate = "/var/lib/mnt_acme/materus.pl/fullchain.pem";
        locations."/" = {
          proxyPass = "http://127.0.0.1:${builtins.toString port}";
          extraConfig = ''
            proxy_set_header    Host                $host;
            proxy_set_header    X-Real-IP           $remote_addr;
            proxy_set_header    X-Forwarded-Ssl     on;
            proxy_set_header    X-Forwarded-For     $proxy_add_x_forwarded_for;
            proxy_set_header    X-Forwarded-Proto   $scheme;
          '';
        };
      };
    };
}
