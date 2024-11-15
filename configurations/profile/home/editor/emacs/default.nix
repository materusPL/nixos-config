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
  packages = epkgs:
    with epkgs; [
      (materus-config epkgs)
      treesit-grammars.with-all-grammars

      use-package
      elcord
      persp-mode
      dashboard
      magit
      git-timemachine
      avy
      vterm
      direnv
      projectile
      clipetty
      which-key
      iedit
      hideshowvis
      evil
      treemacs-evil
      treemacs
      treemacs-nerd-icons
      treemacs-perspective
      treemacs-icons-dired
      treemacs-magit
      treemacs-projectile
      tree-edit
      nerd-icons
      nerd-icons-completion
      perspective
      minions
      rainbow-delimiters
      rainbow-mode
      cmake-mode
      lsp-mode
      lsp-java
      lsp-jedi
      lsp-haskell
      lsp-ui
      lsp-treemacs
      dap-mode
      flycheck
      gradle-mode
      groovy-mode
      kotlin-mode
      d-mode
      lua-mode
      multiple-cursors
      org
      org-contrib
      org-ql
      org-rainbow-tags
      org-roam
      org-roam-ui
      org-review
      org-present
      org-superstar
      org-auto-tangle
      visual-fill-column
      csharp-mode
      markdown-mode
      json-mode
      nix-mode
      no-littering
      right-click-context
      dracula-theme
      doom-themes
      doom-modeline
      popper
      undo-tree
      bash-completion
      eldoc-box
      yasnippet
      async
      request
      nix-ts-mode
      markdown-ts-mode
      llvm-ts-mode
      treesit-fold
      treesit-auto
      tree-sitter-langs
      eat
      vlf
      edit-indirect
      zones
      sudo-edit
      toc-org
      empv
      volatile-highlights
      highlight
      elfeed
      elfeed-goodies
      drag-stuff
      dirvish
      rg
      # Completions & Minibuffer
      corfu
      company
      company-quickhelp
      cape
      embark
      embark-consult
      orderless
      vertico
      marginalia
    ];

  emacsEnv = pkgs.buildEnv {
    name = "emacs-env";
    paths = with pkgs; [
      ripgrep
      cmake
      gnumake
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
      jdk
      gradle
      omnisharp-roslyn
    ];
  };

  cfg = config.materus.profile.editor.emacs;

  setNixInit = ''
    (setenv "PATH" (concat (getenv "PATH") ":${emacsEnv}/bin"))
    (setenv "LD_LIBRARY_PATH" (concat (getenv "LD_LIBRARY_PATH") ":${emacsEnv}/lib"))
    (setq exec-path (append exec-path '("${emacsEnv}/bin")))
    (call-process-shell-command "${pkgs.xorg.xmodmap}/bin/xmodmap -e \"keycode 148 = Hyper_L\" -e \"remove Mod4 = Hyper_L\" -e \"add Mod3 = Hyper_L\" &" nil 0)
    (call-process-shell-command "${pkgs.xorg.xmodmap}/bin/xmodmap -e \"keycode 66 = Hyper_L\" -e \"remove Mod4 = Hyper_L\" -e \"add Mod3 = Hyper_L\" &" nil 0)
  '';
in {
  options.materus.profile.editor.emacs.enable =
    materusArg.pkgs.lib.mkBoolOpt false "Enable emacs with materus cfg";

  config = lib.mkIf cfg.enable {
    home.activation.emacsSetup = lib.hm.dag.entryAfter [ "linkGeneration" ] ''
      mkdir -p ${config.xdg.configHome}/emacs/var/recovery
      mkdir -p ${config.xdg.configHome}/emacs/etc/materus
      mkdir -p ${config.xdg.configHome}/emacs/var/backups
      ${pkgs.rsync}/bin/rsync -zr --no-times --chmod=744 "${configPath}" "${config.xdg.configHome}/emacs"


      run ${config.programs.emacs.finalPackage}/bin/emacs -Q --batch \
          --eval '(setq warning-minimum-log-level :error)' \
          --eval '(setq package-user-dir (concat user-emacs-directory "var/elpa/" emacs-version "/" ))' \
          --eval '(setq package-gnupghome-dir (concat user-emacs-directory "var/elpa/gnupg/" ))' \
          --eval '(package-initialize)' \
          --eval '(byte-recompile-directory (concat user-emacs-directory "etc/materus/extra") 0 t)' \
          --eval '(byte-compile-file "${config.xdg.configHome}/emacs/early-init.el")' \
          --eval '(byte-compile-file "${config.xdg.configHome}/emacs/init.el")' \
          --eval '(byte-compile-file "${config.xdg.configHome}/emacs/nix-init.el")' \
          --eval '(byte-compile-file "${config.xdg.configHome}/emacs/etc/materus/emacs-config.el")'
    '';

    xdg.configFile = { "emacs/nix-init.el".text = setNixInit; };

    #Emacsclient with COLORTERM env variable, without it display in "-nw" client is broken
    xdg.desktopEntries.emacs = {
      name = "Emacs";
      genericName = "Edytor tekstu";
      comment = "Edytuj tekst";
      exec = ''env COLORTERM=truecolor emacsclient -a "" -r -n %F'';
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
        exec = ''env COLORTERM=truecolor emacsclient -a "" -c -n %F'';
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
        #withXwidgets = true;
        withGTK3 = true;
        withAlsaLib = true;
        withGconf = true;
        withImageMagick = true;
      }).overrideAttrs (f: p: {
        #Remove .desktop files, will use my own. Add file with buildtime in case of using elpaca
        postInstall = p.postInstall + ''
          rm -fr $out/share/applications/*
          mkdir -p $out/opt/emacs
          date +%s | tr -d '\n' > $out/opt/emacs/buildtime
        '';
      }));
      extraPackages = epkgs:
        (packages (epkgs.overrideScope (ff: pp: {
          #Build lsp-mode with plist support, need to set this in emacs too
         
          lsp-mode = (pp.lsp-mode.overrideAttrs (f: p: { buildPhase = "export LSP_USE_PLISTS=true\n" + p.buildPhase;}));
          lsp-java = (pp.lsp-java.overrideAttrs (f: p: { buildPhase = "export LSP_USE_PLISTS=true\n" + p.buildPhase;}));
          lsp-jedi= (pp.lsp-jedi.overrideAttrs (f: p: { buildPhase = "export LSP_USE_PLISTS=true\n" + p.buildPhase;}));
          lsp-haskell = (pp.lsp-haskell.overrideAttrs (f: p: { buildPhase = "export LSP_USE_PLISTS=true\n" + p.buildPhase;}));
          lsp-ui  = (pp.lsp-ui.overrideAttrs (f: p: { buildPhase = "export LSP_USE_PLISTS=true\n" + p.buildPhase;}));
          lsp-treemacs  = (pp.lsp-treemacs.overrideAttrs (f: p: { buildPhase = "export LSP_USE_PLISTS=true\n" + p.buildPhase;}));
          dap-mode = (pp.dap-mode.overrideAttrs (f: p: { buildPhase = "export LSP_USE_PLISTS=true\n" + p.buildPhase;}));
        })));
    };

  };
}
