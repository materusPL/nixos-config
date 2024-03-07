{ pkgs, materusArg, lib, ... }:
{
  home.stateVersion = "23.05";
  home.homeDirectory = "/home/materus";

  programs.git.signing.signByDefault = true;

  materus.profile = {
    fonts.enable = lib.mkDefault true;
    nixpkgs.enable = lib.mkDefault false;
    enableDesktop = lib.mkDefault true;
    enableTerminal = lib.mkDefault true;
    enableTerminalExtra = lib.mkDefault true;
    enableNixDevel = lib.mkDefault true;
    editor.code.fhs.enable = true;
    editor.code.fhs.packages = (ps: with ps; let llvmpkgs = llvmPackages_16; in[
      llvmpkgs.clang
      llvmpkgs.llvm
      llvmpkgs.bintools
      llvmpkgs.lld
      llvmpkgs.lldb
      llvmpkgs.libllvm
      llvmpkgs.libllvm.dev

      rustup
      raylib
      gcc
      gdb
      nil
      cmake
      gnumake
      ninja 
      binutils
      coreutils
      util-linux

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
      
      freetype.dev
      
      fpc
      openjdk21
      bison
      flex

      ldc
      dmd
      dub
    ]);

    editor.emacs.enable = false;
    
  };

  home.packages = [
    pkgs.papirus-icon-theme
    materusArg.pkgs.ffmpeg6-amf-full
    (materusArg.pkgs.polymc.wrap { extraJDKs = [ pkgs.graalvm-ce ]; extraLibs = [ ]; })
    pkgs.git-crypt
  ];

  programs.obs-studio = {
    enable = true;
    plugins = with pkgs.obs-studio-plugins; [ wlrobs obs-vaapi obs-vkcapture obs-gstreamer input-overlay obs-multi-rtmp obs-pipewire-audio-capture ];
    package = materusArg.pkgs.obs-amf;
  };
}
