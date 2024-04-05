{ pkgs, materusArg, lib, ... }:
{
  home.stateVersion = "23.05";
  home.homeDirectory = "/home/materus";

  programs.git.signing.signByDefault = true;
  
  xdg.userDirs.enable = true;
  xdg.portal.enable = true;
  xdg.portal.xdgOpenUsePortal = true;
  xdg.portal.extraPortals = [ pkgs.kdePackages.xdg-desktop-portal-kde ];
  xdg.portal.configPackages = [ pkgs.kdePackages.xdg-desktop-portal-kde ];
  materus.profile = {
    fonts.enable = lib.mkDefault true;
    nixpkgs.enable = lib.mkDefault false;
    enableDesktop = lib.mkDefault true;
    enableTerminal = lib.mkDefault true;
    enableTerminalExtra = lib.mkDefault true;
    enableNixDevel = lib.mkDefault true;
    editor.code.fhs.enable = true;
    editor.code.fhs.packages = (ps: with ps; let llvmpkgs = llvmPackages_16; in [
      llvmpkgs.clang
      llvmpkgs.llvm
      llvmpkgs.bintools
      llvmpkgs.lld
      llvmpkgs.lldb
      llvmpkgs.libllvm
      llvmpkgs.libllvm.dev

      raylib
      gcc
      gdb
      nil
      nixfmt
      nixpkgs-fmt
      cmake
      gnumake
      ninja
      binutils
      coreutils
      util-linux
      openssl
      openssl.dev
      pkg-config
      dotnet-sdk_8
      mono
      mold
      python3
      lua
      gtk4.dev
      gtk4
      miniaudio
      SDL2.dev
      SDL2
      freeglut.dev
      freeglut
      boost.dev
      boost
      glew.dev
      libGL.dev
      libGLU.dev
      vulkan-loader.dev
      xorg.xorgproto
      xorg.libX11.dev
      xorg.libXrandr.dev
      xorg.libXrender.dev
      rustup
      freetype.dev

      fpc
      openjdk21
      bison
      flex

      ldc
      dmd
      dub
    ]);

    editor.emacs.enable = true;

  };

  home.packages = [
    pkgs.papirus-icon-theme
    materusArg.pkgs.ffmpeg6-amf-full
    (materusArg.pkgs.polymc.wrap { extraJDKs = [ pkgs.graalvm-ce ]; extraLibs = [ ]; })
    pkgs.git-crypt
  ];

  programs.obs-studio = {
    enable = false;
    plugins = with pkgs.obs-studio-plugins; [ wlrobs obs-vaapi obs-vkcapture obs-gstreamer input-overlay obs-multi-rtmp obs-pipewire-audio-capture ];
    package = materusArg.pkgs.obs-amf;
  };
}
