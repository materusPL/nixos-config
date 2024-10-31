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
    "workbench.colorTheme" = "Tokyo Night";
    "workbench.productIconTheme" = "material-product-icons";
    "workbench.iconTheme" = "material-icon-theme";

    "d.aggressiveUpdate" = false;
    "d.servedPath" = "${pkgs.serve-d}/bin/serve-d";

    "direnv.path.executable" = "${pkgs.direnv}/bin/direnv";

    "nix.enableLanguageServer" = true;
    "nix.serverPath" = "${pkgs.nixd}/bin/nixd";
    "nix.formatterPath" = "${pkgs.nixfmt-classic}/bin/nixfmt";


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
        vscode-marketplace.twxs.cmake
        vscode-extensions.ms-vscode.cpptools
        vscode-marketplace.ms-vscode.cmake-tools
        vscode-marketplace.ms-vscode.cpptools-themes
        #Python
        vscode-marketplace.ms-python.python
        vscode-marketplace.ms-python.vscode-pylance
        vscode-marketplace.ms-python.debugpy
        # CSharp
        vscode-marketplace.ms-dotnettools.csharp
        vscode-marketplace.ms-dotnettools.csdevkit


        #Java
        vscode-marketplace.redhat.java
        vscode-marketplace.vscjava.vscode-java-debug

        #DLang
        vscode-marketplace.webfreak.code-d


        #Nix
        vscode-marketplace.jnoortheen.nix-ide
        vscode-marketplace.arrterian.nix-env-selector

        #Web
        vscode-marketplace.ecmel.vscode-html-css
        vscode-marketplace.formulahendry.auto-close-tag

        #Lua
        vscode-marketplace.sumneko.lua

        #YAML, XML
        vscode-marketplace.redhat.vscode-yaml
        vscode-marketplace.redhat.vscode-xml

        #Git
        vscode-marketplace.donjayamanne.githistory
        vscode-marketplace.mhutchie.git-graph
        vscode-marketplace.eamodio.gitlens

        #Other 
        vscode-marketplace.ms-azuretools.vscode-docker
        vscode-marketplace.webfreak.debug
        vscode-marketplace.mkhl.direnv
        vscode-marketplace.ms-vscode-remote.remote-ssh
        vscode-marketplace.ms-vscode-remote.remote-containers
        vscode-marketplace.esbenp.prettier-vscode
        vscode-marketplace.formulahendry.code-runner
        vscode-marketplace.leonardssh.vscord
        vscode-marketplace.ms-vscode.hexeditor
        vscode-marketplace.alefragnani.project-manager

        #Icons
        vscode-marketplace.pkief.material-icon-theme
        vscode-marketplace.pkief.material-product-icons
        #Themes
        vscode-marketplace.enkia.tokyo-night
        vscode-marketplace.ghgofort.neon-vommit

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
