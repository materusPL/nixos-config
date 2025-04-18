{ config, lib, pkgs, materusArg, materusCfg, ... }:
let
  cfg = config.materus.profile.editor.code;
  ext = let
    market =
      (materusCfg.configInputs.nix-vscode-extensions.extensions."${materusCfg.arch}".forVSCodeVersion
        config.programs.vscode.package.version);
    marketNv =
      (materusCfg.configInputs.nix-vscode-extensions.extensions."${materusCfg.arch}");
  in with market;
  with pkgs; [
    #Cpp
    open-vsx.twxs.cmake
    vscode-extensions.ms-vscode.cpptools
    vscode-marketplace.ms-vscode.cmake-tools
    vscode-marketplace.cs128.cs128-clang-tidy

    #Python
    #vscode-marketplace.ms-python.python
    #vscode-marketplace.ms-python.vscode-pylance
    #vscode-marketplace.ms-python.debugpy

    # CSharp
    open-vsx.muhammad-sammy.csharp

    #Java
    vscode-marketplace.redhat.java
    vscode-marketplace.vscjava.vscode-java-debug
    vscode-marketplace.vscjava.vscode-java-test
    vscode-marketplace.vscjava.vscode-gradle
    vscode-marketplace.vscjava.vscode-java-dependency

    #JS 
    vscode-marketplace.angular.ng-template
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
    #open-vsx.mhutchie.git-graph
    open-vsx.eamodio.gitlens

    #Other 
    open-vsx.ms-azuretools.vscode-docker
    open-vsx.webfreak.debug
    open-vsx.mkhl.direnv
    #vscode-marketplace.ms-vscode-remote.remote-wsl
    #vscode-marketplace.ms-vscode-remote.remote-containers
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
    open-vsx.ahmadawais.shades-of-purple

    #Languages
    marketNv.vscode-marketplace.ms-ceintl.vscode-language-pack-pl

  ];
  set = {
    "vscord.app.name" = lib.mkDefault "VSCodium";
    "window.dialogStyle" = lib.mkDefault "custom";
    "window.titleBarStyle" = lib.mkDefault "custom";
    "editor.fontFamily" =
      lib.mkDefault "'Hack Nerd Font', 'monospace', monospace";
    "workbench.colorTheme" = lib.mkDefault "Shades of Purple";
    "workbench.productIconTheme" = lib.mkDefault "material-product-icons";
    "workbench.iconTheme" = lib.mkDefault "material-icon-theme";

    "d.aggressiveUpdate" = lib.mkDefault false;
    "d.servedPath" = lib.mkDefault "${pkgs.serve-d}/bin/serve-d";

    "direnv.path.executable" = lib.mkDefault "${pkgs.direnv}/bin/direnv";

    "nix.enableLanguageServer" = lib.mkDefault true;
    "nix.serverPath" = lib.mkDefault "${pkgs.nixd}/bin/nixd";
    "nix.formatterPath" = lib.mkDefault "${pkgs.nixfmt-rfc-style}/bin/nixfmt";
    "nix.serverSettings" = {
      "nixd" = { "formatting" = { "command" = lib.mkDefault [ "nixfmt" ]; }; };
    };

    "C_Cpp.clang_format_path" =
      lib.mkDefault "${pkgs.clang-tools}/bin/clang-format";
    "C_Cpp.clang_format_fallbackStyle" = lib.mkDefault "Microsoft";
    "clang-tidy.executable" =
      lib.mkDefault "${pkgs.clang-tools}/bin/clang-tidy";
    "redhat.telemetry.enabled" = lib.mkDefault false;
    "python.defaultInterpreterPath" =
      lib.mkDefault "${pkgs.python3Full}/bin/python";
    "[cpp]" = {
      "editor.defaultFormatter" = lib.mkDefault "xaver.clang-format";
    };
    "cmake.showOptionsMovedNotification" = false;
    "cmake.pinnedCommands" = [
      "workbench.action.tasks.configureTaskRunner"
      "workbench.action.tasks.runTask"
    ];

  };
in {
  options.materus.profile.editor.code.enable =
    materusArg.pkgs.lib.mkBoolOpt config.materus.profile.enableDesktop
    "Enable VSCodium with materus cfg";
  options.materus.profile.editor.code.fhs.enable =
    materusArg.pkgs.lib.mkBoolOpt false "Use fhs vscodium";
  options.materus.profile.editor.code.fhs.packages =
    lib.mkOption { default = (ps: [ ]); };
  options.materus.profile.editor.code.extensions =
    lib.mkOption { default = [ ]; };
  options.materus.profile.editor.code.settings =
    lib.mkOption { default = { }; };
  config = lib.mkIf cfg.enable {
    materus.profile.editor.code.extensions = ext;
    materus.profile.editor.code.settings = set;
    programs.vscode = {
      enable = lib.mkDefault true;
      package = lib.mkDefault (if (cfg.fhs.enable) then
        (pkgs.vscodium.fhsWithPackages cfg.fhs.packages)
      else
        pkgs.vscodium);
      mutableExtensionsDir =
        lib.mkDefault config.materus.profile.editor.code.fhs.enable;
      extensions = lib.mkDefault config.materus.profile.editor.code.extensions;
      enableExtensionUpdateCheck =
        lib.mkDefault config.materus.profile.editor.code.fhs.enable;
      enableUpdateCheck = lib.mkDefault false;
      userSettings = lib.mkDefault config.materus.profile.editor.code.settings;
    };
    materus.profile.fonts.enable = lib.mkDefault true;
  };
}
