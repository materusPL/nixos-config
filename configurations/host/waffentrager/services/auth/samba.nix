{ materusArg, config, lib, pkgs, ... }:
{
  

  config =
    let
      cfg = config.waffentragerService.auth;
      sambaCfg = config.services.samba;
      servicePath = materusArg.waffentrager.samba.servicePath;
      smbToString = x:
        if builtins.typeOf x == "bool"
        then lib.boolToString x
        else builtins.toString x;
      shareConfig = name:
        let share = lib.getAttr name cfg.shares; in
        "[${name}]\n " + (smbToString (
          map
            (key: "${key} = ${smbToString (lib.getAttr key share)}\n")
            (lib.attrNames share)
        ));
    in
    lib.mkIf cfg.enable {
      
      systemd.services.samba-smbd.enable = false;
      systemd.services.samba = {
        description = "Samba Service Daemon";
        requires = [ "rsync-acme.service" ];
        after = [ "rsync-acme.service" ];
        requiredBy = [ "samba.target" ];
        partOf = [ "samba.target" ];

        serviceConfig = {
          ExecStart = "${pkgs.samba4Full}/sbin/samba --foreground --no-process-group";
          ExecReload = "${pkgs.coreutils}/bin/kill -HUP $MAINPID";
          LimitNOFILE = 16384;
          PIDFile = "/run/samba.pid";
          Type = "notify";
          NotifyAccess = "all";
        };
        unitConfig.RequiresMountsFor = servicePath;
      };
      # https://wiki.samba.org/index.php/Samba_AD_DC_Port_Usage
      networking.firewall.allowedTCPPorts = [ 139 445 389 88 53 464 636 3268];
      networking.firewall.allowedUDPPorts = [ 135 137 138 389 88 53 123 464];
      systemd.tmpfiles.rules = [
        "d    ${servicePath}/tls/  0600    root    3000000     -"
        "d    ${servicePath}/private/  0600    root    3000000     -"
        "d    ${servicePath}/lock/  0600    root    3000000     -"
        "d    ${servicePath}/cache/  0600    root    3000000     -"
      ];
      services.samba = {
        enable = true;
        enableNmbd = false;
        enableWinbindd = false;
        package = pkgs.samba4Full;
        configText = ''
          # Global parameters
          [global]
              dns forwarder = ${materusArg.waffentrager.samba.dnsIp}
              netbios name = ${materusArg.waffentrager.samba.netbiosName}
              realm = ${lib.toUpper materusArg.waffentrager.samba.domain}
              server role = active directory domain controller
              workgroup = ${materusArg.waffentrager.samba.workgroup}
              idmap_ldb:use rfc2307 = yes
              ldap server require strong auth = yes
              private dir = ${servicePath}/private
              lock dir = ${servicePath}/lock
              state directory = ${servicePath}/lock
              cache directory = ${servicePath}/cache
              tls enabled  = yes
              tls keyfile  = ${servicePath}/tls/key.pem
              tls certfile = ${servicePath}/tls/fullchain.pem
              tls cafile   = ${servicePath}/tls/chain.pem

          [sysvol]
              path = ${servicePath}/sysvol
              read only = No

          [netlogon]
              path = ${servicePath}/sysvol/${materusArg.waffentrager.samba.domain}/scripts
              read only = No


            ${sambaCfg.extraConfig}

            ${smbToString (map shareConfig (lib.attrNames sambaCfg.shares))}
        '';
      };
    };

}
