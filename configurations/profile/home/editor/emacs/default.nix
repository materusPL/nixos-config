{
  config,
  lib,
  pkgs,
  materusArg,
  ...
}: let
  emacs-pkg = pkgs.emacs;

  materus-nix = e:
    e.trivialBuild {
      pname = "materus-nix";
      src = pkgs.writeText "materus-nix.el" ''
        (when (file-exists-p "${config.programs.emacs.package}/opt/emacs/buildtime")
        (setq emacs-build-time (decode-time (seconds-to-time (string-to-number (with-temp-buffer
        (insert-file-contents "${config.programs.emacs.package}/opt/emacs/buildtime")
        (buffer-string)))))))




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
        (setenv "JAVA_HOME" "${pkgs.jdk21}/lib/openjdk")
        (setenv "PATH" (concat "${emacsEnv}/bin:" (getenv "PATH")))
        (setq exec-path (append '("${emacsEnv}/bin") exec-path))


        (call-process-shell-command "${pkgs.xorg.xmodmap}/bin/xmodmap -e \"keycode 148 = Hyper_L\" -e \"remove Mod4 = Hyper_L\" -e \"add Mod3 = Hyper_L\" &" nil 0)
        (call-process-shell-command "${pkgs.xorg.xmodmap}/bin/xmodmap -e \"keycode 66 = Hyper_L\" -e \"remove Mod4 = Hyper_L\" -e \"add Mod3 = Hyper_L\" &" nil 0)

        (provide 'materus-nix)
      '';
      version = "1.0.0";
    };

  packages = epkgs:
    with epkgs; [
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
      jdk21

      luaformatter
      pandoc
      (luajit.withPackages (p: [
        p.fennel
        p.lua-lsp
      ]))
      fennel-ls
      fnlfmt
      sbcl
      silver-searcher
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
      gradle
      fpc
      nodejs
      omnisharp-roslyn
      texlive.combined.scheme-full
    ];
  };

  cfg = config.materus.profile.editor.emacs;
in {
  options.materus.profile.editor.emacs.enable =
    materusArg.pkgs.lib.mkBoolOpt false "Enable emacs with materus cfg";

  config = lib.mkIf cfg.enable {
    home.activation.emacsSetup = lib.hm.dag.entryAfter ["linkGeneration"] '''';

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
      extraPackages = epkgs: [(materus-nix epkgs) epkgs.vterm epkgs.treesit-grammars.with-all-grammars];
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
        })
        .overrideAttrs
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
    };
  };
}
