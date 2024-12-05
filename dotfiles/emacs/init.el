;;; -*- lexical-binding: t; -*-

;; [[file:../../org-conf/emacs-config.org::*Init][Init:2]]
(setq-default materus/use-nix-packages (require 'materus-config nil 'noerror))
(require 'cl-lib)
(require 'package)

(add-to-list 'load-path (concat user-emacs-directory "etc/materus/extra"))                ; Extra load path for packages
(setq package-user-dir (concat user-emacs-directory "var/elpa/" emacs-version "/" ))      ; Set elpa path for this emacs version, should use nix packages anyway so keeping just in case
(setq package-gnupghome-dir (concat user-emacs-directory "var/elpa/gnupg/" ))             ; Set path to gnupg for elpa
(add-to-list 'package-archives 
             '("nongnu-devel" . "https://elpa.nongnu.org/nongnu-devel/"))                 ; Add nongnu-devel repo to package manager
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)              ; Add melpa repo to package manager
(customize-set-variable 'package-archive-priorities '(("gnu"    . 99)
                                                      ("nongnu" . 80)
                                                      ("nongnu-devel" . 70)
                                                      ("melpa"  . 0)))                    ; Repository priority

(load (concat user-emacs-directory "etc/materus/nix-init") t)
(load (concat user-emacs-directory "etc/materus/emacs-config"))
(load custom-file t)
;; Init:2 ends here
