{
  config,
  pkgs,
  lib,
  ...
}:

{
  home.username = lib.mkDefault "materus";
  home.homeDirectory = lib.mkDefault "/home/materus";




  home.file = {
    ".zshenv".source = "${config.xdg.configFile."zsh/cfg".source}/zshenv";
    ".tmux.conf".source = "${config.mkk.dir}/config/tmux/tmux.conf";
  };

  xdg.enable = true;
  xdg.configFile."wezterm".source = "${config.mkk.dir}/config/terminal/wezterm";
  
  xdg.configFile."zsh/cfg".source =
    "${config.mkk.dir}/config/shell/zsh";
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
  xdg.userDirs.music = lib.mkDefault "${config.xdg.userDirs.extraConfig.AUDIO}/Muzyka";
  xdg.userDirs.pictures = lib.mkDefault "${config.home.homeDirectory}/Obrazy";
  xdg.userDirs.publicShare = lib.mkDefault "${config.home.homeDirectory}/Publiczny";
  xdg.userDirs.templates = lib.mkDefault "${config.home.homeDirectory}/Szablony";
  xdg.userDirs.videos = lib.mkDefault "${config.home.homeDirectory}/Wideo";
  xdg.userDirs.extraConfig = {
    MISC = lib.mkDefault "${config.home.homeDirectory}/Inne";
    PIC_SCREENSHOTS = lib.mkDefault "${config.xdg.userDirs.pictures}/Zrzuty ekranu";
    PIC_MEMES = lib.mkDefault "${config.xdg.userDirs.pictures}/Memy";
    PIC_MISC = lib.mkDefault "${config.xdg.userDirs.pictures}/Inne";
    PIC_PHOTOS = lib.mkDefault "${config.xdg.userDirs.pictures}/Zdjęcia";
    PIC_AVATARS = "${config.xdg.userDirs.pictures}/Avatar";
    AUDIO = lib.mkDefault "${config.home.homeDirectory}/Audio";
    KEYS = lib.mkDefault "${config.xdg.userDirs.documents}/Klucze";
  };
  xdg.userDirs.setSessionVariables = true;

  home.sessionVariables = {
  };

  programs.home-manager.enable = true;
}
