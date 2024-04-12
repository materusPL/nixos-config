{ config, materusArg, lib, pkgs, ... }:
let
  cfg = config.waffentragerService.auth;
in
{
  options.waffentragerService.auth.enable = materusArg.pkgs.lib.mkBoolOpt false "Enable auth";
  imports =
    [
      ./samba.nix
    ];
  config = lib.mkIf cfg.enable
    {
      waffentragerService.elements.enable = true;
      waffentragerService.nginx.enable = true;


      security.acme.defaults.credentialsFile = config.sops.secrets.certs.path;

      systemd.services.resolvconf.enable = false;
      networking.hosts = {
        "${materusArg.ips.wireguard.waffentrager}" = [
          materusArg.waffentrager.samba.domain
          "${materusArg.waffentrager.samba.netbiosName}.${materusArg.waffentrager.samba.domain}"
          materusArg.waffentrager.samba.netbiosName
        ];
      };
      environment.etc = {
        resolvconf = {
          text = ''
            search ${materusArg.waffentrager.samba.domain}
            nameserver ${materusArg.waffentrager.samba.dnsIp}
            nameserver 9.9.9.9
          '';
        };
      };

      systemd.timers.rsync-acme = {
        wantedBy = [ "timers.target" ];
        timerConfig = {
          OnBootSec = "1min";
          OnUnitActiveSec = "1h";
          Unit = "rsync-acme.service";
        };
      };

      systemd.services.rsync-acme = {
        description = "Sync acme for samba";
        path = [ pkgs.rsync ];
        requires = [ "var-lib-mnt_acme.mount" ];
        after = [ "var-lib-mnt_acme.mount" ];
        serviceConfig.Type = "oneshot";
        serviceConfig.RemainAfterExit = false;
        script = ''
          rsync -avzr --chmod=0600 --chown=root:root /var/lib/mnt_acme/${materusArg.waffentrager.samba.domain}/key.pem ${materusArg.waffentrager.samba.servicePath}/tls/
          rsync -avzr --chmod=0640 --chown=root:root /var/lib/mnt_acme/${materusArg.waffentrager.samba.domain}/chain.pem ${materusArg.waffentrager.samba.servicePath}/tls/
          rsync -avzr --chmod=0640 --chown=root:root /var/lib/mnt_acme/${materusArg.waffentrager.samba.domain}/fullchain.pem ${materusArg.waffentrager.samba.servicePath}/tls/
        '';
      };



    };
}
