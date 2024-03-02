{ config, pkgs, lib, materusArg, ... }:
let
  mkBoolOpt = default: description: lib.mkOption {
    inherit default;
    inherit description;
    type = lib.types.bool;
    example = true;
  };

  cfg = config.materus.profile.nixpkgs;
in
{
  options.materus.profile.nixpkgs.enable = mkBoolOpt false "Enable materus nixpkgs config";
  options.materus.profile.nixpkgs.enableOverlays = mkBoolOpt (cfg.enable) "Enable materus overlays";

  config.nixpkgs.config = lib.mkIf cfg.enable{
    allowUnfree = lib.mkDefault true;
    joypixels.acceptLicense = lib.mkDefault true;
    firefox.enablePlasmaBrowserIntegration = true;
  };
  config.nixpkgs.overlays = lib.mkIf cfg.enableOverlays [materusArg.cfg.configInputs.emacs-overlay.overlay];
}
