{ materusArg, config, pkgs, lib, ... }:
let
  cfg = config.materus.profile.browser;
in
{
  options.materus.profile.browser.enable = materusArg.pkgs.lib.mkBoolOpt false "Enable materus default browser config";
  options.materus.profile.browser.default = lib.mkOption {
    type = lib.types.enum [ "firefox" "brave" "vivaldi" ];
    example = "vivaldi";
    default = "brave";
  };
  options.materus.profile.browser.package = materusArg.pkgs.lib.mkPrivateVar
    (if (cfg.default == "firefox") then config.materus.profile.packages.firefox else
    if (cfg.default == "vivaldi") then pkgs.vivaldi else
    if (cfg.default == "brave") then pkgs.brave else { });
  config = lib.mkIf cfg.enable {

    environment.systemPackages = [
      config.materus.profile.browser.package
    ];

  };

}
