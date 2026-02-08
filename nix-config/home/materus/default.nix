{
  config,
  pkgs,
  lib,
  materusArgs,
  ...
}:

{
  home.username = "materus";
  home.homeDirectory = lib.mkDefault "/home/materus";
  home.stateVersion = "25.11";



  home.file = {
    ".zshrc".source = "${config.xdg.configFile."zsh/cfg".source}/zshrc";
    ".zshenv".source = "${config.xdg.configFile."zsh/cfg".source}/zshenv";
  };

  xdg.enable = true;
  xdg.configFile."zsh/cfg".source =
    config.lib.file.mkOutOfStoreSymlink "/mkk/config/config/shell/zsh";
  xdg.configFile."zsh/data/plugins/powerlevel10k".source =
    "${pkgs.zsh-powerlevel10k}/share/zsh-powerlevel10k";
  xdg.configFile."zsh/data/plugins/zsh-history-substring-search".source =
    "${pkgs.zsh-history-substring-search}/share/zsh-history-substring-search";
  xdg.configFile."zsh/data/plugins/zsh-syntax-highlighting".source =
    "${pkgs.zsh-syntax-highlighting}/share/zsh-syntax-highlighting";
  xdg.configFile."zsh/data/plugins/zsh-autosuggestions".source =
    "${pkgs.zsh-autosuggestions}/share/zsh-autosuggestions";

  xdg.configFile."zsh/data/nix.sh".text = ''
    source ${pkgs.vte}/etc/profile.d/vte.sh
  '';


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


  home.sessionVariables = {
  };

  programs.home-manager.enable = true;
}
