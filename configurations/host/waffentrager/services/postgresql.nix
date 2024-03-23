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
      systemd.services.postgresql = {
        partOf = [ "elements-mount.service" ];
        requires = [ "elements-mount.service" ];
        after = [ "elements-mount.service" ];
      };
    };
}
