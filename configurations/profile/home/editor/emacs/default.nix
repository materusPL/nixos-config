{ config, lib, pkgs, materusArg, materusCfg, ... }:
let
  emacs-git =
    materusCfg.configInputs.emacs-overlay.packages.x86_64-linux.emacs-git;

  materus-config = e:
    e.trivialBuild {
      pname = "materus-config";
      src = pkgs.writeText "materus-config.el" ''
        (when (file-exists-p "${config.programs.emacs.package}/opt/emacs/buildtime")
          (setq emacs-build-time (decode-time (seconds-to-time (string-to-number (with-temp-buffer
            (insert-file-contents "${config.programs.emacs.package}/opt/emacs/buildtime")
            (buffer-string)))))))
        (provide 'materus-config)
      '';
      version = "1.0.0";
    };

  configPath = "${materusArg.cfg.path}" + "/extraFiles/config/emacs/";
  inits = import ./init.nix {
    path = configPath;
    inherit pkgs;
  };
  packages = epkgs:
    with epkgs; [
      (materus-config epkgs)
      treesit-grammars.with-all-grammars
    ];

  default-config = ''
    ${setNixInit}
    ${inits.initText}
  '';

  emacsEnv = pkgs.buildEnv {
    name = "emacs-env";
    paths = with pkgs; [
      nixfmt-classic
      python3
      lua
      multimarkdown
      git
      emacs-lsp-booster
      llvmPackages.clang-tools
      llvmPackages.clang
      llvmPackages.lldb
      (hiPrio gcc)
      gdb
      nixd
      jdt-language-server
      omnisharp-roslyn
    ];
  };

  cfg = config.materus.profile.editor.emacs;

  setNixInit = ''
    (setenv "PATH" (concat (getenv "PATH") ":${emacsEnv}/bin"))
    (setq exec-path (append exec-path '("${emacsEnv}/bin")))
    (call-process-shell-command "${pkgs.xorg.xmodmap}/bin/xmodmap -e \"keycode 148 = Hyper_L\" -e \"remove Mod4 = Hyper_L\" -e \"add Mod3 = Hyper_L\" &" nil 0)
    (call-process-shell-command "${pkgs.xorg.xmodmap}/bin/xmodmap -e \"keycode 66 = Hyper_L\" -e \"remove Mod4 = Hyper_L\" -e \"add Mod3 = Hyper_L\" &" nil 0)
  '';
in {
  options.materus.profile.editor.emacs.enable =
    materusArg.pkgs.lib.mkBoolOpt false "Enable emacs with materus cfg";

  config = lib.mkIf cfg.enable {
    home.activation.emacsCompile = lib.hm.dag.entryAfter [ "linkGeneration" ] ''
      mkdir -p ${config.xdg.configHome}/emacs/var/recovery
      mkdir -p ${config.xdg.configHome}/emacs/etc
      
      mkdir -p ${config.xdg.configHome}/emacs/var/backups
      run ${config.programs.emacs.finalPackage}/bin/emacs --batch \
      --eval '(setq warning-minimum-log-level :error)' \
      --eval '(byte-compile-file "${config.xdg.configHome}/emacs/early-init.el")' \
      --eval '(byte-compile-file "${config.xdg.configHome}/emacs/etc/materus/emacs-config.el")' \
      --eval '(byte-compile-file "${config.xdg.configHome}/emacs/init.el")'
    '';

    xdg.configFile = {
      "emacs/early-init.el".text = inits.earlyInitText;
      "emacs/init.el".text = default-config;
      "emacs/etc/materus" = {
        source = configPath + "etc/materus";
        recursive = true;
      };
    };
    xdg.desktopEntries.emacs = {
      name = "Emacs";
      genericName = "Edytor tekstu";
      comment = "Edytuj tekst";
      exec = ''env COLORTERM=truecolor emacsclient -a "" -c %F'';
      icon = "emacs";
      terminal = false;
      type = "Application";
      categories = [ "Development" "TextEditor" ];
      mimeType = [
        "text/english"
        "text/plain"
        "text/x-makefile"
        "text/x-c++hdr"
        "text/x-c++src"
        "text/x-chdr"
        "text/x-csrc"
        "text/x-java"
        "text/x-moc"
        "text/x-pascal"
        "text/x-tcl"
        "text/x-tex"
        "application/x-shellscript"
        "text/x-c"
        "text/x-c++"
        "x-scheme-handler/org-protocol"
      ];
      actions.new-window = {
        exec = ''env COLORTERM=truecolor emacsclient -a "" -c %F'';
        name = "Nowe okno";
      };
      actions.no-daemon = {
        exec = "env COLORTERM=truecolor emacs %F";
        name = "Instancja samodzielna";
      };
    };

    programs.emacs = {
      enable = true;
      package = lib.mkDefault ((emacs-git.override {
        withSQLite3 = true;
        withWebP = true;
        withX = true;
        withGTK3 = true;
        withAlsaLib = true;
        withGconf = true;
        withImageMagick = true;
      }).overrideAttrs (f: p: {
        postInstall = p.postInstall + ''
          rm -fr $out/share/applications/*
          mkdir -p $out/opt/emacs
          date +%s | tr -d '\n' > $out/opt/emacs/buildtime
        '';
      }));
      extraPackages = epkgs: (packages epkgs);
    };

  };
}
