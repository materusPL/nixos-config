{ config, pkgs, lib, materusArg, ... }:
{
  options.valkyrieService.pihole.enable = materusArg.pkgs.lib.mkBoolOpt false "Enable pihole";
  options.valkyrieService.pihole.dnsIP = lib.mkOption { default = "127.0.0.1";};
  options.valkyrieService.pihole.webIP = lib.mkOption { default = "127.0.0.1";};


  
  config = let 
  cfg = config.valkyrieService.pihole;
  dnsmasqConf = pkgs.writeText "02-dnsmasq-custom.conf" ''
  no-hosts
  '';

  in lib.mkIf config.valkyrieService.pihole.enable {
    systemd.tmpfiles.rules = [
      "d    /var/lib/dnsmasq.d   0776    root    root     -"
      "d    /var/lib/pihole   0776    root    root     -"
      "L+   /var/lib/dnsmasq.d/02-dnsmasq-custom.conf  0776 root root - ${dnsmasqConf}"
    ];

    virtualisation.oci-containers.containers.pihole = {
      image = "pihole/pihole:latest";
      ports =
        [
          "${cfg.dnsIP}:53:53/tcp"
          "${cfg.dnsIP}:53:53/udp"
          "${cfg.webIP}:3000:80"
        ];
      environment = {
        TZ = "Europe/Warsaw";
        FTLCONF_LOCAL_IPV4="127.0.0.1";
        DNSMASQ_USER="root";
        VIRTUAL_HOST="pi.hole";
        PROXY_LOCATION="pi.hole";
      };
      volumes = [
        "/var/lib/pihole/:/etc/pihole/"
        "/var/lib/dnsmasq.d:/etc/dnsmasq.d/"
        "/nix/store:/nix/store"
      ];
      extraOptions =
        [
          "--cap-add=NET_ADMIN"
          "--dns=127.0.0.1"
          "--dns=9.9.9.9"
          "--hostname=pi.hole"
        ];
    };

  };


}
