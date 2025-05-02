{ pkgs, materusArg, lib, ... }:
{
  imports = [
    ./plasma.nix
  ];
  home.stateVersion = "23.05";
  home.homeDirectory = "/home/materus";
  
  programs.git.signing.signByDefault = true;

  xdg.userDirs.enable = true;
  materus.profile = {
    
    fonts.enable = lib.mkDefault true;
    nixpkgs.enable = lib.mkDefault false;
    enableDesktop = lib.mkDefault true;
    enableTerminal = lib.mkDefault true;
    enableTerminalExtra = lib.mkDefault true;
    enableNixDevel = lib.mkDefault true;
    editor.emacs.enable = true;
    editor.code.fhs.enable = true;
    editor.code.fhs.packages = (ps: with ps; let llvmpkgs = llvmPackages_18; in [
      llvmpkgs.clang
      llvmpkgs.llvm
      llvmpkgs.bintools
      llvmpkgs.lld
      llvmpkgs.lldb
      llvmpkgs.libllvm
      llvmpkgs.mlir
      llvmpkgs.libllvm.dev

      fpc
      xmake
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
      glib
      glib.dev
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
      vulkan-headers
      xorg.xorgproto
      xorg.libX11.dev
      xorg.libXrandr.dev
      xorg.libXrender.dev
      rustup
      freetype.dev

      fpc
      gradle
      bison
      flex

      ldc
      dmd
      dub
    ]);

    

  };

  home.packages = [
    materusArg.pkgs.ffmpeg_7-amf-full
    (materusArg.pkgs.polymc-qt5.wrap { extraJDKs = [ pkgs.graalvm-ce ]; extraLibs = [ ]; })
    pkgs.git-crypt
    pkgs.obsidian
  ];

  programs.obs-studio = {
    enable = true;
    plugins = with pkgs.obs-studio-plugins; [
      wlrobs
      input-overlay

      obs-source-switcher
      obs-tuna
      obs-vaapi
      obs-vkcapture
      obs-gstreamer
      obs-backgroundremoval
      obs-multi-rtmp
      obs-pipewire-audio-capture
    ];
  };

  home.file.".gradle/gradle.properties".text = ''
    org.gradle.java.installations.fromEnv=JAVA_8_HOME,JAVA_17_HOME,JAVA_21_HOME
    org.gradle.home=${pkgs.jdk21}
  '';

  xdg.desktopEntries.brave-browser =
    let
      env = lib.concatStringsSep " " [
      ];
      script = pkgs.writeShellScript "brave" ''
        ${env} brave "$@"
      '';

    in
    {
      name = "Brave Web Browser";
      genericName = "Przeglądarka WWW";
      comment = "Skorzystaj z internetu";
      exec = "${script} %U";
      icon = "brave-browser";
      terminal = false;
      categories = [ "Application" "Network" "WebBrowser" ];
      mimeType = [
        "application/pdf"
        "application/rdf+xml"
        "application/rss+xml"
        "application/xhtml+xml"
        "application/xhtml_xml"
        "application/xml"
        "image/gif"
        "image/jpeg"
        "image/png"
        "image/webp"
        "text/html"
        "text/xml"
        "x-scheme-handler/http"
        "x-scheme-handler/https"
        "x-scheme-handler/ipfs"
        "x-scheme-handler/ipns"
      ];
      actions.new-windows = {
        exec = "${script}";
        name = "Nowe okno";
      };
      actions.new-private-windows = {
        exec = "${script} --incognito";
        name = "Nowe okno incognito";
      };
    };
}
