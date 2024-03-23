{ config, pkgs, lib, materusArg, ... }:
let
  packages = config.materus.profile.packages;
  cfg = config.materus.profile.fonts;
in
{
  options.materus.profile.fonts.enable = materusArg.pkgs.lib.mkBoolOpt false "Enable materus font settings for OS";

  config = lib.mkIf cfg.enable {

    fonts.packages = packages.list.fonts ++ packages.list.moreFonts;
    fonts.enableDefaultPackages = lib.mkDefault true;

    fonts.fontconfig.enable = lib.mkDefault true;
    fonts.fontconfig.cache32Bit = lib.mkDefault true;

    fonts.fontconfig.defaultFonts.sansSerif = [ "Noto Sans" "DejaVu Sans" "WenQuanYi Zen Hei" "Noto Color Emoji" ];
    fonts.fontconfig.defaultFonts.serif = [ "Noto Serif" "DejaVu Serif" "WenQuanYi Zen Hei" "Noto Color Emoji" ];
    fonts.fontconfig.defaultFonts.emoji = [ "Noto Color Emoji" "OpenMoji Color" ];
    fonts.fontconfig.defaultFonts.monospace = [ "FiraCode Nerd Font Mono" "Noto Sans Mono" "WenQuanYi Zen Hei Mono" ];

    fonts.fontDir.enable = lib.mkDefault true;
  };
}
