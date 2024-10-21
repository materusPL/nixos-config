{ config, lib, materusArg, pkgs, ... }:
let
  cfg = config.materus.profile.wezterm;
  zshCfg = ''
    source "${config.programs.wezterm.package}/etc/profile.d/wezterm.sh"
  '';
in
{
  options.materus.profile.wezterm.enable = materusArg.pkgs.lib.mkBoolOpt false "Enable materus wezterm config";
  options.materus.profile.wezterm.enableHackFont = materusArg.pkgs.lib.mkBoolOpt true "Enable hack nerd font for wezterm";
  options.materus.profile.wezterm.enableWezcraft = materusArg.pkgs.lib.mkBoolOpt true "Enable alias to start wezcraft with monocraft font";
  options.materus.profile.wezterm.extraConfig = lib.mkOption {
    default = "";
    description = "Config for wezterm";
    type = lib.types.lines;
  };
  config = lib.mkIf cfg.enable
    {
      programs.wezterm.enable = true;
      programs.wezterm.colorSchemes = { };
      programs.wezterm.enableZshIntegration = false;
      programs.wezterm.extraConfig = ''

      package.path = package.path .. ";${materusArg.cfg.path}/extraFiles/config/wezterm/?.lua"
      require("wezterm_config");
      local config = materus_wezterm_config();
      ${lib.optionalString cfg.enableHackFont "config.font = wezterm.font 'Hack Nerd Font';"}
      ${cfg.extraConfig}

      return config;
    '';

      home.packages = [
        (lib.mkIf cfg.enableHackFont (pkgs.nerdfonts.override {
          fonts = [ "Hack" ];
        }))
        (lib.mkIf cfg.enableWezcraft (pkgs.monocraft))
        (lib.mkIf cfg.enableWezcraft (pkgs.writeShellScriptBin "wezcraft" ''
          ${lib.getExe config.programs.wezterm.package} --config font="wezterm.font 'Monocraft Nerd Font'" $@
        ''))
      ];

      materus.profile.zsh.endConfig = lib.optionalString cfg.enableWezcraft zshCfg;
    };
}
