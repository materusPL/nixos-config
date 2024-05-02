{ config, pkgs, materusArg, lib, ... }:
{
  home.stateVersion = "22.11";
  home.homeDirectory = "/home/materus";
  programs.wezterm.enable = true;
  programs.git = {
    userEmail = "materus@podkos.pl";
    userName = "materus";
    signing.signByDefault = true;
    signing.key = "${materusArg.cfg.path}/extraFiles/keys/ssh/materus.pub";
    extraConfig = {
      commit.gpgsign = true;
      gpg.format = "ssh";
    };
  };
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


      gcc
      gdb

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

      jdk


      ldc
      dmd
      dub
    ]);

    editor.emacs.enable = false;

  };
  xdg.desktopEntries.brave-browser = let env = lib.concatStringsSep " " [
    ''VK_ICD_FILENAMES=''$VK_ICD_FILENAMES:/run/opengl-driver/share/vulkan/icd.d/nvidia_icd.x86_64.json:/run/opengl-driver-32/share/vulkan/icd.d/nvidia_icd.i686.json''
    ''__NV_PRIME_RENDER_OFFLOAD="1"''
    ''__NV_PRIME_RENDER_OFFLOAD_PROVIDER="NVIDIA-G0"''
    ''__GLX_VENDOR_LIBRARY_NAME="nvidia"''
    ''__VK_LAYER_NV_optimus="NVIDIA_only"''
    ''NIXOS_OZONE_WL="1"''
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
  home.packages = [
    pkgs.papirus-icon-theme
    (materusArg.pkgs.polymc-qt5.wrap { withWaylandGLFW=true; extraJDKs = [ pkgs.graalvm-ce ]; })
  ];

}
