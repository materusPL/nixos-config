(setq-default materus/use-nix-packages (require 'materus-config nil 'noerror))
(cua-mode 0) ; To load cua variables/functions but not set it up yet
(load (concat user-emacs-directory "etc/materus/emacs-config"))
(when (file-exists-p custom-file)
  (load custom-file))
