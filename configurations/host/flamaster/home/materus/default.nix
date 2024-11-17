{ config, pkgs, materusArg, lib, ... }:
{
  home.stateVersion = "23.05";
  home.homeDirectory = "/home/materus";

  materus.profile = {
    fonts.enable = lib.mkDefault true;
    nixpkgs.enable = lib.mkDefault false;
    enableDesktop = lib.mkDefault true;
    enableTerminal = lib.mkDefault true;
    enableTerminalExtra = lib.mkDefault true;
    enableNixDevel = lib.mkDefault true;
    
    bash.enable = true;
    zsh.enable = true;
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
      materusArg.unstable.nixd
      nixfmt-rfc-style
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

  };
  
  home.packages = [
    pkgs.papirus-icon-theme
    (materusArg.pkgs.polymc.wrap { extraJDKs = [ pkgs.graalvm-ce ]; })
  ];

}
