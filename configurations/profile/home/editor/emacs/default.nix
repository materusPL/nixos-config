{ config, lib, pkgs, materusArg, ... }:
let
  packages =  epkgs: with epkgs; [
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

        lsp-mode
        lsp-bridge
        dap-mode
        d-mode
        multiple-cursors
        org
        org-rainbow-tags
        markdown-mode
        json-mode
        nix-mode
        
        minimap
        moe-theme
      ];


  default-config = ''
  (defvar materus/nix-packages t)
  (defvar materus/init-from-home nil)
  (defvar materus/init-from-default nil)
  (when (not materus/init-from-home)
          (setq-default materus/init-from-default t)
          (message "Config loading not from homeDir, need "materus/init-from-home" variable in init.el")
          ${setNixInit}
          (require 'materus-config)
        )
  '';



  materus-config = epkgs: epkgs.trivialBuild rec {
        pname = "materus-config";
        src = pkgs.symlinkJoin {
          name = "materus-emacs-config";
          paths = [
            configPath
          ];
          };
        version = "1.0";
        packageRequires = (packages epkgs);
        buildPhase = ''
          runHook preBuild

          emacs -L . --batch -f batch-byte-compile **/*.el
          emacs -L . --batch -f batch-byte-compile *.el

          runHook postBuild
        '';

        installPhase = ''
          runHook preInstall

          LISPDIR=$out/share/emacs/site-lisp
          install -d $LISPDIR
          install **.el **.elc $LISPDIR
          cp -r materus $LISPDIR
          emacs --batch -l package --eval "(package-generate-autoloads \"${pname}\" \"$LISPDIR\")"

          runHook postInstall
        '';
      };



  cfg = config.materus.profile.editor.emacs;
  configPath = "${materusArg.cfg.path}" + "/extraFiles/config/emacs/"; 
  emacsPkgs = with pkgs;[
    python3
    lua
    multimarkdown
    git
  ];
  setNixInit = ''
    (setenv "PATH" (concat (getenv "PATH") ":${lib.makeBinPath emacsPkgs}"))
    ${builtins.concatStringsSep "\n" (builtins.map (x: "(setq exec-path (append exec-path '(\""+x+"/bin\")))" ) emacsPkgs)}
    (call-process-shell-command "${pkgs.xorg.xmodmap}/bin/xmodmap -e \"keycode 66 = Hyper_L\" -e \"remove Mod4 = Hyper_L\" -e \"add Mod3 = Hyper_L\" &" nil 0)
  '';
in
{
  options.materus.profile.editor.emacs.enable = materusArg.pkgs.lib.mkBoolOpt false "Enable emacs with materus cfg";

  config = lib.mkIf cfg.enable {
    xdg.configFile."emacs/init.el".text = ''
    (defvar materus/nix-packages nil)
    (defvar materus/init-from-home t)
    (defvar materus/init-from-default nil)
    ${setNixInit}
    (setq-default materus/init-from-home t)
    (setq-default materus/nix-packages (require 'materus-config nil 'noerror))
    (when (not materus/nix-packages)
      (load (concat  user-emacs-directory "materus/init"))
      (message "Config loaded from user dir")
    )
    '';
    xdg.configFile."emacs/materus" = {
      source = configPath + "materus";
      recursive = true;
    };
    programs.emacs = {
      enable = true;
      package = with pkgs; lib.mkDefault (emacs29.override { withX = true; withGTK3 = true; withAlsaLib = true; withGconf = true; withImageMagick = true; withXwidgets = true; });
      extraPackages = epkgs: ((packages epkgs)  ++ [(materus-config epkgs)]);
      extraConfig = default-config; 
    };

  };
}
