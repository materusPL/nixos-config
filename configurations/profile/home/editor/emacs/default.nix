{ config, lib, pkgs, materusArg, ... }:
let
  cfg = config.materus.profile.editor.emacs;
  configPath = "${materusArg.cfg.path}" + "/extraFiles/config/emacs/"; 
  emacsPkgs = with pkgs;[
    python3
    lua
  ];
in
{
  options.materus.profile.editor.emacs.enable = materusArg.pkgs.lib.mkBoolOpt false "Enable emacs with materus cfg";

  config = lib.mkIf cfg.enable {
    programs.emacs = {
      enable = true;
      package = with pkgs; lib.mkDefault (if pkgs ? emacs-pgtk then emacs-pgtk else emacs29-gtk);
      extraPackages = epkgs: with epkgs; [
        evil
        evil-numbers
        evil-mc
        evil-tex
        evil-nerd-commenter

        dashboard
        magit
        helm
        avy
        corfu
        ivy
        vterm
        centaur-tabs
        treemacs
        treemacs-evil
        treemacs-nerd-icons
        tree-edit
        doom-modeline
        nerd-icons
        nerd-icons-completion
        load-relative

        lsp-mode
        d-mode

        org
        markdown-mode
        json-mode

        moe-theme
      ];
      extraConfig = ''
        ;;;; Set emacs PATH
        
        (setenv "PATH" (concat (getenv "PATH") ":${lib.makeBinPath emacsPkgs}"))
        ${builtins.concatStringsSep "\n" (builtins.map (x: "(setq exec-path (append exec-path '(\""+x+"/bin\")))" ) emacsPkgs)}
      
        ;;;; Done setting PATH

        ;;;; Config
        ${builtins.readFile (configPath + "config.el")}
        ;;;; Config End
      '';
    };

  };
}
