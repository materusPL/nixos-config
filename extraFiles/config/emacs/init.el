;;; -*- lexical-binding: t; -*-

(setq-default materus/use-nix-packages (require 'materus-config nil 'noerror))
(load (concat user-emacs-directory "etc/materus/nix-init") t)
(load (concat user-emacs-directory "etc/materus/emacs-config"))
(load custom-file t)
