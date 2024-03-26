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
      environment.systemPackages = [ pkgs.samba pkgs.exiftool pkgs.ffmpeg-headless ];
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
        extraApps = with pkgs.nextcloud28Packages.apps; {
          inherit notify_push previewgenerator;
        };
        extraOptions = {
          mail_smtpmode = "sendmail";
          mail_sendmailmode = "pipe";
        };
        globalProfiles = true;

        phpOptions = {
          "opcache.interned_strings_buffer" = "10";
          "opcache.jit" = "1255";
          "opcache.jit_buffer_size" = "128M";
          "opcache.revalidate_freq" = "60";
          "opcache.save_comments" = "1";
        };
        phpExtraExtensions = ex: [ ex.zip ex.zlib ex.tidy ];
      };
      services.nginx.virtualHosts.${config.services.nextcloud.hostName} = {
        forceSSL = true;
        http3 = true;
        sslTrustedCertificate = "/var/lib/mnt_acme/materus.pl/chain.pem";
        sslCertificateKey = "/var/lib/mnt_acme/materus.pl/key.pem";
        sslCertificate = "/var/lib/mnt_acme/materus.pl/fullchain.pem";
        extraConfig = ''
          proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        '';
      };
    };
}
