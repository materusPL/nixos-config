{ config, lib, pkgs, materusArg, ... }:
let
  cfg = config.materus.profile.browser;
  osConfig = (if (builtins.hasAttr "osConfig" config._module.args) then config._module.args.osConfig else null);
in
{

  options = let mkBoolOpt = materusArg.pkgs.lib.mkBoolOpt; in {
    materus.profile.browser.firefox.enable = mkBoolOpt false "Enable Firefox with materus cfg";
    materus.profile.browser.vivaldi.enable = mkBoolOpt false "Enable Vivaldi with materus cfg";
    materus.profile.browser.brave.enable = mkBoolOpt false "Enable Brave with materus cfg";

  };
  #TODO: Make some config
  config = lib.mkMerge [{
      home.packages = [
      (lib.mkIf cfg.firefox.enable config.materus.profile.packages.firefox)
      (lib.mkIf cfg.vivaldi.enable pkgs.vivaldi)
      (lib.mkIf cfg.brave.enable pkgs.brave)
    ];
  }
  ];


}
