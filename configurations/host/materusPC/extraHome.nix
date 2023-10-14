{ config, pkgs, materusPkgs, lib, ... }:
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


      gcc
      gdb

      cmake
      mold
      python3
      lua
      SDL2.dev
      SDL2
      boost.dev
      boost
      fpc
      jdk


      ldc
      dmd
      dub
    ]);

  };

  home.packages = [
    pkgs.papirus-icon-theme
    materusPkgs.ffmpeg6-amf-full
    (materusPkgs.polymc.wrap { extraJDKs = [ pkgs.graalvm-ce ]; extraLibs = [ ]; })
  ];

  programs.obs-studio = {
    enable = true;
    plugins = with pkgs.obs-studio-plugins; [ wlrobs obs-vaapi obs-vkcapture obs-gstreamer input-overlay obs-multi-rtmp obs-pipewire-audio-capture ];
    package = materusPkgs.obs-amf;
  };
}
