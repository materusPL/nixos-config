{ config, lib, materusArg, pkgs, ... }:
let
  cfg = config.materus.profile.wezterm;
in
{
  options.materus.profile.wezterm.enable = materusArg.pkgs.lib.mkBoolOpt config.materus.profile.enableDesktop "Enable materus wezterm config";
  options.materus.profile.wezterm.enableHackFont = materusArg.pkgs.lib.mkBoolOpt true "Enable hack nerd font for wezterm";
  options.materus.profile.wezterm.extraConfig = lib.mkOption {
    default = "";
    description = "Config for wezterm";
    type = lib.types.lines;
  };
  config = lib.mkIf cfg.enable {
    programs.wezterm.enable = true;
    programs.wezterm.colorSchemes = { };
    programs.wezterm.extraConfig = ''

      package.path = package.path .. ";${materusArg.cfg.path}/extraFiles/config/wezterm/?.lua"
      require("wezterm_config");
      local config = materus_wezterm_config();
      ${cfg.extraConfig}

      return config;
    '';

    home.packages = lib.mkIf cfg.enableHackFont [ (pkgs.nerdfonts.override { fonts = [ "Hack" ]; }) ];
  };
}
