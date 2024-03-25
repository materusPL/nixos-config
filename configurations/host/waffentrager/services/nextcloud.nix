{ materusArg, config, lib, pkgs, ... }:
{
  options.waffentragerService.nextcloud.enable = materusArg.pkgs.lib.mkBoolOpt false "Enable nextcloud";

  config =
    let
      cfg = config.waffentragerService.nextcloud;
    in
    lib.mkIf cfg.enable {
      waffentragerService.elements.enable = true;
      waffentragerService.postgresql.enable = true;
      waffentragerService.nginx.enable = true;

      sops.secrets.nextcloud-adminpass.owner = config.users.users.nextcloud.name;
      sops.secrets.nextcloud-adminpass.group = config.users.users.nextcloud.group;

      services.postgresql.ensureDatabases = [ "nextcloud" ];
      services.postgresql.ensureUsers = [{
        name = "nextcloud";
        ensureDBOwnership = true;
      }];
      services.nextcloud = {
        enable = true;
        notify_push.enable = true;
        package = pkgs.nextcloud28;
        hostName = "waffentrager.materus.pl";
        home = config.waffentragerService.elements.nextcloudDir;
        config.adminuser = "master";
        config.adminpassFile = config.sops.secrets.nextcloud-adminpass.path;
        config.dbtype = "pgsql";
        config.defaultPhoneRegion = "PL";
        config.trustedProxies = [ materusArg.ips.valkyrie materusArg.ips.wireguard.valkyrie materusArg.ips.wireguard.waffentrager ];
        extraAppsEnable = true;
        maxUploadSize = "4G";
        https = true;
        enableImagemagick = true;
        configureRedis = true;
        webfinger = true;
        appstoreEnable = true;
        database.createLocally = true;
        nginx.recommendedHttpHeaders = true;
        extraApps = { notify_push = pkgs.nextcloud28Packages.apps.notify_push; };
        extraOptions = {
          mail_smtpmode = "sendmail";
          mail_sendmailmode = "pipe";
        };
      };
      services.nginx.virtualHosts.${config.services.nextcloud.hostName} = {
        addSSL = true;
        http2 = false;
        sslTrustedCertificate = "/var/lib/mnt_acme/materus.pl/chain.pem";
        sslCertificateKey = "/var/lib/mnt_acme/materus.pl/key.pem";
        sslCertificate = "/var/lib/mnt_acme/materus.pl/fullchain.pem";
        extraConfig = ''
          proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        '';
      };
    };
}