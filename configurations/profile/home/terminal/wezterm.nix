{ config, lib, materusArg, pkgs, ... }:
let
  cfg = config.materus.profile.wezterm;
  cfgText = ''
    config.hide_tab_bar_if_only_one_tab = true;
    config.enable_scroll_bar = true;
    config.font = wezterm.font 'Hack Nerd Font';
  '';
in
{
  options.materus.profile.wezterm.enable = materusArg.pkgs.lib.mkBoolOpt config.materus.profile.enableDesktop "Enable materus wezterm config";
  options.materus.profile.wezterm.enableHackFont = materusArg.pkgs.lib.mkBoolOpt true "Enable hack nerd font for wezterm";
  options.materus.profile.wezterm.config = lib.mkOption {
    default = cfgText;
    description = "Config for wezterm";
    type = lib.types.string;
  };
  config = lib.mkIf cfg.enable {
    programs.wezterm.enable = true;
    programs.wezterm.colorSchemes = { };
    programs.wezterm.extraConfig = ''
      local config = wezterm.config_builder();

      ${cfgText}

      return config;
    '';

    home.packages = lib.mkIf cfg.enableHackFont [ (pkgs.nerdfonts.override { fonts = [ "Hack" ]; }) ];
  };
}
