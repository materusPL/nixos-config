{ materusArg, config, lib, ... }:
let
  cfg = config.materus.profile.tmux;
in
{
  options.materus.profile.tmux.enable = materusArg.pkgs.lib.mkBoolOpt true "Enable materus tmux config";
  config = lib.mkIf cfg.enable
    {
      programs.tmux = {
        enable = true;
        clock24 = lib.mkDefault true;
        aggressiveResize = lib.mkDefault true;
        escapeTime = lib.mkDefault 0;
        historyLimit = lib.mkDefault 10000;
        mouse = lib.mkDefault true;
        terminal = lib.mkDefault "tmux-256color";
      };

    };
}
