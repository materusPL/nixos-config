{ config, lib, pkgs, materusArg, ... }:
let 
cfg = config.materus.profile.browser;
in
{
  
  options= let mkBoolOpt = materusArg.pkgs.lib.mkBoolOpt; in{
    materus.profile.browser.firefox.enable = mkBoolOpt config.materus.profile.enableDesktop "Enable Firefox with materus cfg";
    materus.profile.browser.vivaldi.enable = mkBoolOpt config.materus.profile.enableDesktop "Enable Vivaldi with materus cfg";
    materus.profile.browser.brave.enable = mkBoolOpt  false "Enable Brave with materus cfg";

  };
  #TODO: Make some config
  config.home.packages = [
  (lib.mkIf cfg.firefox.enable config.materus.profile.packages.firefox)
  (lib.mkIf cfg.vivaldi.enable pkgs.vivaldi)
  (lib.mkIf cfg.brave.enable pkgs.brave)
  ];
  

}