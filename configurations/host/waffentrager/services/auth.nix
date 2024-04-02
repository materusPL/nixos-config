{ materusArg, config, lib, pkgs, ... }:
{
  options.waffentragerService.auth.enable = materusArg.pkgs.lib.mkBoolOpt false "Enable auth";

  config =
    let
      cfg = config.auth.postgresql;
    in
    lib.mkIf cfg.enable {
      waffentragerService.elements.enable = true;
      waffentragerService.nginx.enable = true;

      services.postgresql.enable = true;
      services.postgresql.package = pkgs."postgresql_${cfg.version}";
      services.postgresql.dataDir = "${config.waffentragerService.elements.postgresqlDir}/${cfg.version}";
      services.postgresql.enableJIT = true;
      services.postgresql.authentication = pkgs.lib.mkOverride 10 ''
        local all all trust
        host all all 127.0.0.1/32 scram-sha-256
      '';
      systemd.services.postgresql = {
        partOf = [ "elements-mount.service" ];
        requires = [ "elements-mount.service" ];
        after = [ "elements-mount.service" ];
      };
    };
}