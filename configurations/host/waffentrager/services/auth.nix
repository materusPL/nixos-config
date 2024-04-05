{ materusArg, config, lib, pkgs, ... }:
{
  options.waffentragerService.auth.enable = materusArg.pkgs.lib.mkBoolOpt false "Enable auth";

  config =
    let
      cfg = config.waffentragerService.auth;
    in
    lib.mkIf cfg.enable {
      waffentragerService.elements.enable = true;
      waffentragerService.nginx.enable = true;

    };
}