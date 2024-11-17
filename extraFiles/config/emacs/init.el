;;; -*- lexical-binding: t; -*-

(setq-default materus/use-nix-packages (require 'materus-config nil 'noerror))
(unless  (file-directory-p (concat user-emacs-directory "var/quickstart")) 
  (make-directory (concat user-emacs-directory "var/quickstart") t))
(load (concat user-emacs-directory "etc/materus/nix-init") t)
(load (concat user-emacs-directory "etc/materus/emacs-config"))
(load custom-file t)
