{
  config,
  lib,
  pkgs,
  materusArg,
  materusCfg,
  ...
}:
let
  emacs-pkg = pkgs.emacs;

  materus-config =
    e:
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
  packages =
    epkgs: with epkgs; [
      (materus-config epkgs)
      treesit-grammars.with-all-grammars
      use-package
      elcord
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
      perspective
      treemacs
      treemacs-perspective
      treemacs-nerd-icons
      treemacs-icons-dired
      treemacs-magit
      treemacs-projectile
      tree-edit
      nerd-icons
      nerd-icons-completion
      minions
      rainbow-delimiters
      rainbow-mode
      cmake-mode
      lsp-mode
      lsp-java
      lsp-jedi
      lsp-haskell
      lsp-pascal
      lsp-pyright
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
      org-modern
      org-auto-tangle
      ox-pandoc
      visual-fill-column
      csharp-mode
      markdown-mode
      json-mode
      nix-mode
      nixfmt
      nix-ts-mode
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
      yasnippet-capf
      async
      request
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
      #empv
      volatile-highlights
      highlight
      elfeed
      elfeed-goodies
      drag-stuff
      dirvish
      rg
      shfmt
      mermaid-mode
      ob-mermaid
      visual-replace
      scroll-restore
      highlight-indent-guides
      diff-hl
      transient
      embark
      embark-consult
      ef-themes
      pdf-tools
      minimap
      geiser-guile
      fennel-mode
      paredit
      # Completions & Minibuffer
      corfu
      corfu-terminal
      kind-icon
      cape
      orderless
      vertico
      marginalia
    ];

  emacsEnv = pkgs.buildEnv {
    name = "emacs-env";
    paths = with pkgs; [
      luaformatter
      pandoc
      (luajit.withPackages (p: [p.fennel p.lua-lsp]))
      fennel-ls
      fnlfmt
      
      guile
      plantuml
      mermaid-cli
      pyright
      shfmt
      ripgrep
      cmake
      gnumake
      nixfmt-rfc-style
      python3Full
      multimarkdown
      git
      emacs-lsp-booster
      llvmPackages.clang-tools
      llvmPackages.clang
      llvmPackages.lldb
      (hiPrio gcc)
      gdb
      materusArg.unstable.nixd
      jdt-language-server
      jdk
      gradle
      fpc
      nodejs
      omnisharp-roslyn
      texlive.combined.scheme-full
    ];
  };

  cfg = config.materus.profile.editor.emacs;

  setNixInit = ''
    (defvar lsp-java-configuration-runtimes nil)
    (setq dap-lldb-debug-program '("${pkgs.llvmPackages.lldb}/bin/lldb-vscode"))
    (setq lsp-java-configuration-runtimes '[(:name "JavaSE-1.8"
                                                   :path "${pkgs.jdk8}/lib/openjdk/")
                                            (:name "JavaSE-17"
                                                   :path "${pkgs.jdk17}/lib/openjdk/")
                                            (:name "JavaSE-21"
                                                   :path "${pkgs.jdk21}/lib/openjdk/"
                                                   :default t)])
    (setq lsp-nix-nixd-nixos-options-expr (concat "(builtins.getFlake \"/etc/nixos\").nixosConfigurations." (system-name) ".options"))                                           
    (setenv "PATH" (concat (getenv "PATH") ":${emacsEnv}/bin"))
    (setq exec-path (append exec-path '("${emacsEnv}/bin")))
    (call-process-shell-command "${pkgs.xorg.xmodmap}/bin/xmodmap -e \"keycode 148 = Hyper_L\" -e \"remove Mod4 = Hyper_L\" -e \"add Mod3 = Hyper_L\" &" nil 0)
    (call-process-shell-command "${pkgs.xorg.xmodmap}/bin/xmodmap -e \"keycode 66 = Hyper_L\" -e \"remove Mod4 = Hyper_L\" -e \"add Mod3 = Hyper_L\" &" nil 0)
  '';
in
{
  options.materus.profile.editor.emacs.enable = materusArg.pkgs.lib.mkBoolOpt false "Enable emacs with materus cfg";

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
      --eval '(byte-compile-file "${config.xdg.configHome}/emacs/etc/materus/nix-init.el")' \
      --eval '(byte-compile-file "${config.xdg.configHome}/emacs/etc/materus/emacs-config.el")'
    '';

    xdg.configFile = {
      "emacs/etc/materus/nix-init.el".text = setNixInit;
    };

    #Emacsclient with COLORTERM env variable, without it display in "-nw" client is broken
    xdg.desktopEntries.emacs = {
      name = "Emacs";
      genericName = "Edytor tekstu";
      comment = "Edytuj tekst";
      exec = ''env COLORTERM=truecolor emacsclient -a "" -r %F'';
      icon = "emacs";
      terminal = false;
      type = "Application";
      categories = [
        "Development"
        "TextEditor"
      ];
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
      package = lib.mkDefault (
        (emacs-pkg.override {
          withSQLite3 = true;
          withWebP = true;
          withX = true;
          #withXwidgets = true;
          withGTK3 = true;
          withAlsaLib = true;
          #withGconf = true;
          withImageMagick = true;
        }).overrideAttrs
          (
            f: p: {
              #Remove .desktop files, will use my own. Add file with buildtime in case of using elpaca
              postInstall =
                p.postInstall
                + ''
                  rm -fr $out/share/applications/*
                  mkdir -p $out/opt/emacs
                  date +%s | tr -d '\n' > $out/opt/emacs/buildtime
                '';
            }
          )
      );
      extraPackages =
        epkgs:
        (packages (
          epkgs.overrideScope (
            ff: pp: {
              #Build lsp-mode with plist support, need to set this in emacs too

              lsp-mode = (
                pp.lsp-mode.overrideAttrs (
                  f: p: {
                    patches = [ ./lsp-mode.patch ];
                    buildPhase =
                      ''
                        export LSP_USE_PLISTS=true
                      ''
                      + p.buildPhase;
                  }
                )
              );
              lsp-java = (
                pp.lsp-java.overrideAttrs (
                  f: p: {
                    buildPhase =
                      ''
                        export LSP_USE_PLISTS=true
                      ''
                      + p.buildPhase;
                  }
                )
              );
              lsp-jedi = (
                pp.lsp-jedi.overrideAttrs (
                  f: p: {
                    buildPhase =
                      ''
                        export LSP_USE_PLISTS=true
                      ''
                      + p.buildPhase;
                  }
                )
              );
              lsp-haskell = (
                pp.lsp-haskell.overrideAttrs (
                  f: p: {
                    buildPhase =
                      ''
                        export LSP_USE_PLISTS=true
                      ''
                      + p.buildPhase;
                  }
                )
              );
              lsp-ui = (
                pp.lsp-ui.overrideAttrs (
                  f: p: {
                    buildPhase =
                      ''
                        export LSP_USE_PLISTS=true
                      ''
                      + p.buildPhase;
                  }
                )
              );
              lsp-treemacs = (
                pp.lsp-treemacs.overrideAttrs (
                  f: p: {
                    buildPhase =
                      ''
                        export LSP_USE_PLISTS=true
                      ''
                      + p.buildPhase;
                  }
                )
              );
              dap-mode = (
                pp.dap-mode.overrideAttrs (
                  f: p: {
                    buildPhase =
                      ''
                        export LSP_USE_PLISTS=true
                      ''
                      + p.buildPhase;
                  }
                )
              );
            }
          )
        ));
    };

  };
}
