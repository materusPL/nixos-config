{ config, pkgs, lib, materusArg, ... }:
let
  packages = config.materus.profile.packages;
  cfg = config.materus.profile.fonts;
in
{
  options.materus.profile.fonts.enable = materusArg.pkgs.lib.mkBoolOpt config.materus.profile.enableDesktop "Enable materus font settings";

  config = lib.mkIf cfg.enable {
    fonts.fontconfig.enable = lib.mkDefault true;
    home.packages = packages.list.fonts;
  };
}
