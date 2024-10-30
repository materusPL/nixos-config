(setq-default materus/nix-packages (require 'doom-themes nil 'noerror))
(unless materus/nix-packages (load (concat user-emacs-directory "etc/materus/packages")))
(load (concat user-emacs-directory "etc/materus/internals"))
(load (concat user-emacs-directory "etc/materus/start"))
