{ config, pkgs, materusArg, materusCfg, lib, ... }:
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

  programs.vscode.userSettings = {
    "vscord.app.name" = "VSCodium";
    "window.dialogStyle" = "custom";
    "window.titleBarStyle" = "custom";
    "editor.fontFamily" = "'Hack Nerd Font', 'monospace', monospace";
    "workbench.colorTheme" = "Dracula Theme";
    "workbench.productIconTheme" = "material-product-icons";
    "workbench.iconTheme" = "material-icon-theme";

    "d.aggressiveUpdate" = false;
    "d.servedPath" = "${pkgs.serve-d}/bin/serve-d";

    "direnv.path.executable" = "${pkgs.direnv}/bin/direnv";

    "nix.enableLanguageServer" = true;
    "nix.serverPath" = "${pkgs.nixd}/bin/nixd";
    "nix.formatterPath" = "${pkgs.nixfmt-classic}/bin/nixfmt";

    "clang-format.executable" = "${pkgs.clang-tools}/bin/clang-format";
    "clang-tidy.executable" =  "${pkgs.clang-tools}/bin/clang-tidy";

    "python.defaultInterpreterPath"= "${pkgs.python3Full}/bin/python";

  };

  materus.profile = {
    fonts.enable = lib.mkDefault true;
    nixpkgs.enable = lib.mkDefault false;
    enableDesktop = lib.mkDefault true;
    enableTerminal = lib.mkDefault true;
    enableTerminalExtra = lib.mkDefault true;
    enableNixDevel = lib.mkDefault true;
    editor.code.fhs.enable = false;
    editor.code.fhs.extensions =
      let
        ext = (materusCfg.configInputs.nix-vscode-extensions.extensions."${materusCfg.arch}".forVSCodeVersion
          config.programs.vscode.package.version);
      in
      with ext;
      with pkgs;
      [
        #Cpp
        open-vsx.twxs.cmake
        vscode-extensions.ms-vscode.cpptools
        vscode-marketplace.ms-vscode.cmake-tools
        open-vsx.xaver.clang-format
        vscode-marketplace.cs128.cs128-clang-tidy

        #Python
        vscode-marketplace.ms-python.python
        vscode-marketplace.ms-python.vscode-pylance
        vscode-marketplace.ms-python.debugpy

        # CSharp
        vscode-marketplace.ms-dotnettools.csharp
        vscode-marketplace.ms-dotnettools.csdevkit


        #Java
        open-vsx.redhat.java
        open-vsx.vscjava.vscode-java-debug

        #DLang
        open-vsx.webfreak.code-d


        #Nix
        open-vsx.jnoortheen.nix-ide

        #Web
        open-vsx.ecmel.vscode-html-css
        open-vsx.formulahendry.auto-close-tag

        #Lua
        open-vsx.sumneko.lua

        #YAML, XML
        open-vsx.redhat.vscode-yaml
        open-vsx.redhat.vscode-xml

        #Git
        open-vsx.donjayamanne.githistory
        open-vsx.mhutchie.git-graph
        open-vsx.eamodio.gitlens

        #Other 
        open-vsx.ms-azuretools.vscode-docker
        open-vsx.webfreak.debug
        open-vsx.mkhl.direnv
        vscode-marketplace.ms-vscode-remote.remote-ssh
        vscode-marketplace.ms-vscode-remote.remote-containers
        open-vsx.esbenp.prettier-vscode
        open-vsx.formulahendry.code-runner
        open-vsx.leonardssh.vscord
        open-vsx.ms-vscode.hexeditor
        open-vsx.alefragnani.project-manager
        vscode-marketplace.cantonios.project-templates

        #Icons
        open-vsx.pkief.material-icon-theme
        open-vsx.pkief.material-product-icons
        #Themes
        open-vsx.dracula-theme.theme-dracula

        #Languages
        vscode-marketplace.ms-ceintl.vscode-language-pack-pl

      ];
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
  xdg.desktopEntries.brave-browser =
    let
      env = lib.concatStringsSep " " [
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
    (materusArg.pkgs.polymc-qt5.wrap { withWaylandGLFW = true; extraJDKs = [ pkgs.graalvm-ce ]; })
  ];

}
