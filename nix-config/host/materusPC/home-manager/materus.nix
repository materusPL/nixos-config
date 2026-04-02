{
  pkgs,
  materusArgs,
  config,
  lib,
  ...
}:
let
  jsonFormat = pkgs.formats.json { };
in
{
  mkk.neovim.enable = true;

  mkk.dir = config.lib.file.mkOutOfStoreSymlink "/mkk/config";
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
  home.packages = with pkgs; [
    materusArgs.inputs.nixerus.packages.x86_64-linux.polymc
    neovide

    curl
    wget
    python3

    packwiz
    ani-cli
    mpv
    kitty

    libreoffice-qt6-fresh

    direnv
    jsonnet
    jsonnet-language-server
  ];

  programs.vscode = {
    enable = true;
    mutableExtensionsDir = true;
    profiles.default.enableExtensionUpdateCheck = true;
    profiles.default.extensions = with pkgs.nix-vscode-extensions.vscode-marketplace; [
      # VSCode
      kerrickstaley.layered-settings
      mkhl.direnv
      betterthantomorrow.joyride
      dracula-theme.theme-dracula
      leonardssh.vscord
      ms-vscode.hexeditor
      pkief.material-icon-theme
      pkief.material-product-icons

      # Python
      #ms-python.vscode-pylance
      ms-python.python
      ms-python.debugpy
      ms-python.vscode-python-envs

      # Git
      eamodio.gitlens
      donjayamanne.githistory
      waderyan.gitblame
      codezombiech.gitignore

      # Lua
      sumneko.lua

      # Jsonnet
      grafana.vscode-jsonnet

      # Nix
      jnoortheen.nix-ide

      # C++
      twxs.cmake
      llvm-vs-code-extensions.vscode-clangd
      ms-vscode.cmake-tools
      cs128.cs128-clang-tidy
      xaver.clang-format

      # Other
      redhat.vscode-yaml
      redhat.vscode-xml
      webfreak.debug
    ];
    package = (
      pkgs.vscodium.fhsWithPackages (
        ps: with ps; [
          nixfmt-rfc-style
          nixd
          direnv
          jsonnet
          jsonnet-language-server
          clang-tools
          clang
        ]
      )
    );
  };

  xdg.configFile."VSCodium/User/settings.nix.jsonnet".enable = false;
  home.activation.mutableFileGeneration =
    let
      source = jsonFormat.generate "settings.nix" {
        # VSCode
        "window.dialogStyle" = "custom";
        "window.titleBarStyle" = "custom";
        "workbench.colorTheme" = "Dracula Theme";
        "workbench.iconTheme" = "material-icon-theme";
        "editor.fontFamily" = "'Hack Nerd Font', 'monospace', monospace";
        "direnv.path.executable" = "${pkgs.direnv}/bin/direnv";
        "vscord.app.name" = "VSCodium";

        # Typescript
        "typescript.tsserver.maxTsServerMemory" = 1024 * 8;
        "typescript.tsserver.nodePath" = "${pkgs.nodejs}/bin/node";
        # Nix
        "[nix]" = {
          "editor.defaultFormatter" = "jnoortheen.nix-ide";
        };
        "nix.enableLanguageServer" = true;
        "nix.formatterPath" = "${pkgs.nixfmt-rfc-style}/bin/nixfmt";
        "nix.serverPath" = "${pkgs.nixd}/bin/nixd";
        "nix.serverSettings" = {

          "nixd" = {
            "nixpkgs" = {
              "expr" = "import (builtins.getFlake \"/mkk/config\").inputs.nixpkgs { }";
            };
            "formatting" = {
              "command" = [
                "nixfmt"
              ];
            };
            "options" = {
              "nixos" = {
                "expr" =
                  "(builtins.getFlake (builtins.toString \"/mkk/config\" )).nixosConfigurations.materusPC.options";
              };
              "home-manager" = {
                "expr" =
                  "(builtins.getFlake (builtins.toString \"/mkk/config\")).homeConfigurations.materus.options";
              };
            };
          };
        };
        # C++
        "C_Cpp.clang_format_path" = "${pkgs.clang-tools}/bin/clang-format";
        "C_Cpp.clang_format_fallbackStyle" = "Microsoft";
        "clang-tidy.executable" = "${pkgs.clang-tools}/bin/clang-tidy";
        "[cpp]" = {
          "editor.defaultFormatter" = "xaver.clang-format";
        };
        "[c]" = {
          "editor.defaultFormatter" = "xaver.clang-format";
        };
        "cmake.showOptionsMovedNotification" = false;
        "cmake.pinnedCommands" = [
          "workbench.action.tasks.configureTaskRunner"
          "workbench.action.tasks.runTask"
        ];
        "clang-format.fallbackStyle" = "Microsoft";

      };
      target = config.xdg.configFile."VSCodium/User/settings.nix.jsonnet".target;
      command = ''
        echo "Copying mutable home files for $HOME"
        $VERBOSE_ECHO "${source} -> ${target}"
        $DRY_RUN_CMD cp --remove-destination --no-preserve=mode ${source} ${target}
      '';
    in
    (lib.hm.dag.entryAfter [ "linkGeneration" ] command);

  xdg.dataFile."java-runtimes/graalvm-oracle-17".source = pkgs.graalvmPackages.graalvm-oracle_17;
  xdg.dataFile."java-runtimes/graalvm-oracle-latest".source = pkgs.graalvmPackages.graalvm-oracle;
  xdg.dataFile."java-runtimes/openjdk21".source = pkgs.jdk21;
}
