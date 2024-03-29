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

      xdg.userDirs.enable = lib.mkDefault true;
      xdg.userDirs.createDirectories = lib.mkDefault config.xdg.userDirs.enable;

      xdg.userDirs.desktop = lib.mkDefault "${config.home.homeDirectory}/Pulpit";
      xdg.userDirs.documents = lib.mkDefault "${config.home.homeDirectory}/Dokumenty";
      xdg.userDirs.download = lib.mkDefault "${config.home.homeDirectory}/Pobrane";
      xdg.userDirs.music = lib.mkDefault "${config.xdg.userDirs.extraConfig.XDG_AUDIO_DIR}/Muzyka";
      xdg.userDirs.pictures = lib.mkDefault "${config.home.homeDirectory}/Obrazy";
      xdg.userDirs.publicShare = lib.mkDefault "${config.home.homeDirectory}/Publiczny";
      xdg.userDirs.templates = lib.mkDefault "${config.home.homeDirectory}/Szablony";
      xdg.userDirs.videos = lib.mkDefault "${config.home.homeDirectory}/Wideo";
      xdg.userDirs.extraConfig = {
        XDG_MISC_DIR = lib.mkDefault "${config.home.homeDirectory}/Inne";
        XDG_PIC_SCREENSHOTS_DIR = lib.mkDefault "${config.xdg.userDirs.pictures}/Zrzuty ekranu";
        XDG_PIC_MEMES_DIR = lib.mkDefault "${config.xdg.userDirs.pictures}/Memy";
        XDG_PIC_MISC_DIR = lib.mkDefault "${config.xdg.userDirs.pictures}/Inne";
        XDG_PIC_PHOTOS_DIR = lib.mkDefault "${config.xdg.userDirs.pictures}/Zdjęcia";
        XDG_PIC_AVATARS_DIR = "${config.xdg.userDirs.pictures}/Avatar";
        XDG_AUDIO_DIR = lib.mkDefault "${config.home.homeDirectory}/Audio";
        XDG_KEYS_DIR = lib.mkDefault "${config.xdg.userDirs.documents}/Klucze";
      };
    };


}
