{ config, lib, pkgs, materusPkgs, materusFlake, ... }:
let
  cfg = config.materus.profile.editor.emacs;
  configPath = "${materusFlake.selfPath}" + "/extraFiles/config/emacs/materus/"; 
  emacsPkgs = with pkgs;[
    python3
    lua
  ];
in
{
  options.materus.profile.editor.emacs.enable = materusPkgs.lib.mkBoolOpt false "Enable emacs with materus cfg";

  config = lib.mkIf cfg.enable {
    #TODO: Make config
    /*home.activation.doomEmacs = lib.hm.dag.entryBetween [ "onFilesChange" ] [ "writeBoundry" ] ''
      if [ ! -d ~/.emacs.d ] ; 
      then ${pkgs.git}/bin/git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.emacs.d 
      fi
      PATH="${config.programs.git.package}/bin:${config.programs.emacs.package}/bin:$PATH"
      ~/.emacs.d/bin/doom sync
      '';

      home.file.doomEmacs.source = "${materusArg.flakeData.extraFiles}/config/emacs/doom";
      home.file.doomEmacs.target = "${config.xdg.configHome}/doom";*/

    programs.emacs = {
      enable = true;
      package = with pkgs; lib.mkDefault (if pkgs ? emacs-pgtk then emacs-pgtk else emacs-gtk);
      extraPackages = epkgs: with epkgs; [
        evil
        evil-numbers
        evil-mc
        evil-tex
        evil-nerd-commenter

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

        vscode-dark-plus-theme
        kaolin-themes
      ];
      extraConfig = ''
        ;;;; Set emacs PATH
        
        (setenv "PATH" (concat (getenv "PATH") ":${lib.makeBinPath emacsPkgs}"))
        ${builtins.concatStringsSep "\n" (builtins.map (x: "(setq exec-path (append exec-path '(\""+x+"/bin\")))" ) emacsPkgs)}
      
        ;;;; Done setting PATH

        ; Load Config file
         (load-file "${configPath + "config.el"}")
      '';
    };

  };
}
