{ materusArg, config, lib, pkgs, ... }:
{
  options.waffentragerService.postgresql.enable = materusArg.pkgs.lib.mkBoolOpt false "Enable postgresql";
  options.waffentragerService.postgresql.version = lib.mkOption { default = "16"; };

  config =
    let
      cfg = config.waffentragerService.postgresql;
    in
    lib.mkIf cfg.enable {
      waffentragerService.elements.enable = true;

      services.postgresql.enable = true;
      services.postgresql.package = pkgs."postgresql_${cfg.version}";
      services.postgresql.dataDir = "${config.waffentragerService.elements.postgresqlDir}/${cfg.version}";
      services.postgresql.enableJIT = true;
      services.postgresql.authentication = pkgs.lib.mkOverride 10 ''
        local all all trust
        host all all 127.0.0.1/32 scram-sha-256
        host all all ::1/128 scram-sha-256
      '';
      systemd.services.postgresql = {
        requires = [ "elements-mount.service" ];
        after = [ "elements-mount.service" ];
      };
    };
}
