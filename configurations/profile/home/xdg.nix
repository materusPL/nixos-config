{ config, lib, materusArg, pkgs, ... }:
{

  options.materus.profile.xdg =
    {
      enable = materusArg.pkgs.lib.mkBoolOpt config.materus.profile.enableDesktop "Enable xdg settings";
    };

  config =
    let
      cfg = config.materus.profile.xdg;
    in
    lib.mkIf cfg.enable { 
      xdg.enable = true;
      xdg.portal.enable = lib.mkDefault false;
      xdg.portal.xdgOpenUsePortal = lib.mkDefault config.xdg.portal.enable;
      
      xdg.userDirs.enable = lib.mkDefault true;
      xdg.userDirs.createDirectories = lib.mkDefault config.xdg.userDirs.enable;

      xdg.userDirs.desktop = lib.mkDefault "${config.home.homeDirectory}/Pulpit";
      xdg.userDirs.documents = lib.mkDefault "${config.home.homeDirectory}/Dokumenty";
      xdg.userDirs.download = lib.mkDefault "${config.home.homeDirectory}/Pobrane";
      xdg.userDirs.music = lib.mkDefault "${config.home.homeDirectory}/Muzyka";
      xdg.userDirs.pictures = lib.mkDefault "${config.home.homeDirectory}/Obrazy";
      xdg.userDirs.publicShare = lib.mkDefault "${config.home.homeDirectory}/Publiczny";
      xdg.userDirs.templates = lib.mkDefault "${config.home.homeDirectory}/Szablony";
      xdg.userDirs.videos = lib.mkDefault "${config.home.homeDirectory}/Wideo";
      xdg.userDirs.extraConfig = {
        XDG_MISC_DIR = lib.mkDefault "${config.home.homeDirectory}/Inne";
        XDG_SCREENSHOTS_DIR = "${config.xdg.userDirs.pictures}/Zrzuty ekranu";
      };
    };


}
