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
        package = pkgs.nextcloud29;
        hostName = "waffentrager.materus.pl";
        home = config.waffentragerService.elements.nextcloudDir;
        config.adminuser = "nextcloud-master";
        config.adminpassFile = config.sops.secrets.nextcloud-adminpass.path;
        config.dbtype = "pgsql";
        extraAppsEnable = true;
        maxUploadSize = "8G";
        https = true;
        enableImagemagick = true;
        configureRedis = true;
        webfinger = true;
        appstoreEnable = true;
        database.createLocally = true;
        nginx.recommendedHttpHeaders = true;
        extraApps = with pkgs.nextcloud29Packages.apps; {
          inherit notify_push previewgenerator;
        };
        settings = {
          "profile.enabled" = true;
          default_phone_region = "PL";
          trusted_proxies = [ materusArg.ips.valkyrie materusArg.ips.wireguard.valkyrie materusArg.ips.wireguard.waffentrager ];
          mail_smtpmode = "sendmail";
          mail_sendmailmode = "pipe";
          enable_previews = true;
          preview_format = "webp";
          enabledPreviewProviders = [
            ''OC\Preview\Movie''
            ''OC\Preview\PNG''
            ''OC\Preview\JPEG''
            ''OC\Preview\GIF''
            ''OC\Preview\BMP''
            ''OC\Preview\XBitmap''
            ''OC\Preview\MP3''
            ''OC\Preview\OGG''
            ''OC\Preview\OPUS''
            ''OC\Preview\MP4''
            ''OC\Preview\TXT''
            ''OC\Preview\MarkDown''
            ''OC\Preview\PDF''
            ''OC\Preview\WebP''
            ''OC\Preview\OpenDocument''
            ''OC\Preview\Krita''
            ''OC\Preview\AVIF''
          ];
          "overwrite.cli.url" = "https://${config.services.nextcloud.hostName}";
        };

        phpOptions = {
          "opcache.memory_consumption" = "512";
          "opcache.interned_strings_buffer" = "64";
          "opcache.max_accelerated_files"="50000";
          "opcache.jit" = "1255";
          "opcache.jit_buffer_size" = "128M";
          "opcache.validate_timestamps" = "0";
          "opcache.revalidate_freq" = "0";
          "opcache.fast_shutdown" = "1";
          "opcache.save_comments" = "1";
        };
        phpExtraExtensions = ex: [ ex.zip ex.zlib ex.tidy ex.smbclient ];
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
