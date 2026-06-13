{ pkgs, ... }:
{
  home.stateVersion = "26.05";
  mkk.neovim.enable = true;

  home.packages = with pkgs; [
    neovide
    obsidian
    git-crypt

    telegram-desktop
    discord
    spotify
    remmina

    mesa-demos
    vulkan-tools
    nixfmt

    curl
    wget
    python3

    packwiz
    ani-cli
    mpv
    kitty
    keepassxc
    moonlight-qt
    wezterm

    (vivaldi.override {proprietaryCodecs = true;})
  ];
  programs.vscode = {
    enable = true;
    mutableExtensionsDir = true;
  };
  programs.git = {
    enable = true;
    lfs.enable = true;
    settings = {
      user.email = "materus@podkos.pl";
      user.name = "materus";
      commit.gpgsign = true;
      gpg.format = "ssh";
    };

    signing.signByDefault = true;
    signing.key = "/mkk/config/extra-files/ssh/materus.pub";
  };
}
