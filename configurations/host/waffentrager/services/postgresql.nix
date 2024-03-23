{ materusArg, config, lib, pkgs, ... }:
{
  options.waffentragerService.postgresql.enable = materusArg.pkgs.lib.mkBoolOpt false "Enable postgresql";


  config =
    let
      cfg = config.waffentragerService.postgresql;
    in
    lib.mkIf cfg.enable {
      waffentragerService.elements.enable = true;

      services.postgresql.enable = true;
      services.postgresql.package = pkgs.postgresql_16;
      services.postgresql.dataDir = "${config.waffentragerService.elements.path}/services/postgresql";
      systemd.services.postgresql = {
        partOf = ["elements-mount.service"];
        requires = ["elements-mount.service"];
        after = ["elements-mount.service"];
      };
    };
}
