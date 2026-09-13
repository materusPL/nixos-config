{
  pkgs,
  config,
  materusArgs,
  ...
}:
{
  home.stateVersion = "26.05";
  mkk.dir = config.lib.file.mkOutOfStoreSymlink "/mkk/config";

  mkk.neovim.enable = true;
  home.packages = with pkgs; [

    neovide
    obsidian
    git-crypt
    tmux

    telegram-desktop
    discord
    spotify
    remmina

    mesa-demos
    vulkan-tools
    nixfmt
    nixd

    eza

    curl
    wget
    python3

    packwiz
    ani-cli

    kitty
    keepassxc
    moonlight-qt
    wezterm
    (pkgs.writeShellApplication {
      name = "soundblaster-mic";
      runtimeInputs = with pkgs; [
        bash
        coreutils
        alsa-utils
      ];
      text = "exec bash ${materusArgs.files.scripts.audio.setmic}";
    })
    floorp-bin
    (firefox.override {
      extraPrefsFiles = [
        (builtins.fetchurl {
          url = "https://raw.githubusercontent.com/MrOtherGuy/fx-autoconfig/master/program/config.js";
          sha256 = "1mx679fbc4d9x4bnqajqx5a95y1lfasvf90pbqkh9sm3ch945p40";
        })
      ];
    })
  ];
  programs.mpv = {
    enable = true;
    config = {
      ytdl-format = "bestvideo+bestaudio";
      slang = "pl,pol,Polish,Polski,en,eng,English";
      alang = "ja,jp,jpn,Japanese,pl,pol,Polski,en,eng,English";
      demuxer-max-bytes = "500MiB";
      demuxer-max-back-bytes = "150MiB";
      cache = "yes";
      cache-pause-wait = "10";
      cache-pause-initial = "yes";
      keep-open = "yes";
    };
  };

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
  programs.delta = {
    enable = true;
    enableGitIntegration = true;
  };
  programs.gitui.enable = true;
  programs.yt-dlp.enable = true;
}
