{ config, lib, pkgs, materusArg, ... }:
let
  cfg = config.materus.profile.editor.code;
in
{
  options.materus.profile.editor.code.enable = materusArg.pkgs.lib.mkBoolOpt config.materus.profile.enableDesktop "Enable VSCodium with materus cfg";
  options.materus.profile.editor.code.fhs.enable = materusArg.pkgs.lib.mkBoolOpt false "Use fhs vscodium";
  options.materus.profile.editor.code.fhs.packages = lib.mkOption { default = (ps: [ ]); };
  options.materus.profile.editor.code.fhs.extensions = lib.mkOption { default = []; };
  config = lib.mkIf cfg.enable {
    programs.vscode = {
      enable = lib.mkDefault true;
      package = lib.mkDefault (if (cfg.fhs.enable) then (pkgs.vscodium.fhsWithPackages cfg.fhs.packages) else pkgs.vscodium);
      mutableExtensionsDir = lib.mkDefault config.materus.profile.editor.code.fhs.enable;
      extensions = lib.mkDefault  config.materus.profile.editor.code.fhs.extensions;
      enableExtensionUpdateCheck = lib.mkDefault  config.materus.profile.editor.code.fhs.enable;
      enableUpdateCheck = lib.mkDefault false;
    };
    materus.profile.fonts.enable = lib.mkDefault true;
  };
}
