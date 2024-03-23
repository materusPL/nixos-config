{ materusArg, config, lib, ... }:
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
      };

      systemd.services.nginx = {
        requires = [ "var-lib-mnt_acme.mount" ];
        after = [ "var-lib-mnt_acme.mount" ];
      };
    };

}
