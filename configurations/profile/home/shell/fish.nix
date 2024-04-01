{ config, pkgs, lib, materusArg, ... }:
let
  cfg = config.materus.profile.fish;
in
{
  options.materus.profile.fish.enable = materusArg.pkgs.lib.mkBoolOpt config.materus.profile.enableTerminalExtra "Enable materus fish config";
  config = lib.mkIf cfg.enable {
    programs.fish = {
      enable =  true;
    };
  };
}
