{ config, lib, pkgs, materusArg, ... }:
let
  configPath = "${materusArg.cfg.path}" + "/extraFiles/config/emacs/";

  inits = import ./init.nix {
    path = configPath;
    inherit pkgs;
  };
  packages = epkgs:
    with epkgs; [
      load-relative
      elcord
      persp-mode
      dashboard
      magit
      helm
      avy
      corfu
      vterm
      centaur-tabs
      projectile
      company
      clipetty

      treemacs
      treemacs-nerd-icons
      treemacs-perspective
      treemacs-icons-dired
      treemacs-magit
      treemacs-projectile
      tree-edit
      vertico
      nerd-icons
      nerd-icons-completion
      perspective
      minions
      telephone-line
      rainbow-delimiters
      use-package

      cmake-mode
      lsp-mode
      lsp-java
      lsp-jedi
      lsp-haskell
      lsp-ui
      lsp-treemacs
      dap-mode
      d-mode
      multiple-cursors
      org
      org-rainbow-tags
      org-roam
      org-roam-ui
      org-review
      markdown-mode
      json-mode
      nix-mode
      no-littering

      moe-theme
      doom-themes
    ];

  default-config = ''
    ${setNixInit}
    ${inits.initText}
  '';

  emacsPkgs = with pkgs; [ python3 lua multimarkdown git ];

  cfg = config.materus.profile.editor.emacs;

  setNixInit = ''
    (setenv "PATH" (concat (getenv "PATH") ":${lib.makeBinPath emacsPkgs}"))
    ${builtins.concatStringsSep "\n" (builtins.map
      (x: ''(setq exec-path (append exec-path '("'' + x + ''/bin")))'')
      emacsPkgs)}
    (call-process-shell-command "${pkgs.xorg.xmodmap}/bin/xmodmap -e \"keycode 148 = Hyper_L\" -e \"remove Mod4 = Hyper_L\" -e \"add Mod3 = Hyper_L\" &" nil 0)
  '';
in {
  options.materus.profile.editor.emacs.enable =
    materusArg.pkgs.lib.mkBoolOpt false "Enable emacs with materus cfg";

  config = lib.mkIf cfg.enable {
    home.activation.emacsCompile = lib.hm.dag.entryAfter [ "linkGeneration" ] ''
      mkdir -p ${config.xdg.configHome}/emacs/var/backups
      mkdir -p ${config.xdg.configHome}/emacs/var/recovery
      mkdir -p ${config.xdg.configHome}/emacs/etc
      run ${config.programs.emacs.finalPackage}/bin/emacs --batch \
      --eval '(setq warning-minimum-log-level :error)' \
      --eval '(byte-compile-file "${config.xdg.configHome}/emacs/early-init.el")' \
      --eval '(byte-compile-file "${config.xdg.configHome}/emacs/init.el")'
    '';

    xdg.configFile."emacs/early-init.el".text = ''
      ${inits.earlyInitText}
    '';
    xdg.configFile."emacs/init.el".text =''
    (require 'recentf)
    (require 'no-littering)
    (add-to-list 'recentf-exclude
                (recentf-expand-file-name no-littering-var-directory))
    (add-to-list 'recentf-exclude
                (recentf-expand-file-name no-littering-etc-directory))
             '';

    programs.emacs = {
      enable = true;
      package = with pkgs;
        lib.mkDefault (pkgs.emacs29.override {
          withSQLite3 = true;
          withWebP = true;
          withX = true;
          withGTK3 = true;
          withAlsaLib = true;
          withGconf = true;
          withImageMagick = true;
          withXwidgets = true;
        });
      extraPackages = epkgs: ((packages epkgs));
      extraConfig = default-config;
    };

  };
}
