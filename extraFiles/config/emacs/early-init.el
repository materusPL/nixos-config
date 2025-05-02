;;; -*- lexical-binding: t; -*-
  
;;; VARIABLES


(setenv "LSP_USE_PLISTS" "true")                                                          ; Make lsp-mode use plists

(setq c-default-style nil)                                                                ; Clear default styles for languages, will set them up later
(setq default-input-method nil)                                                           ; Disable default input method, I'm not using them anyway so far
(setq initial-major-mode 'fundamental-mode)                                               ; Use fundamental mode in scratch buffer, speed up loading, not really important when emacs used as daemon
(setq auto-save-default nil)                                                              ; TODO: configure auto saves, disable for now
(setq backup-directory-alist
      `((".*" . ,(concat user-emacs-directory "var/backups/"))))                          ; Set backup location
(setq auto-save-file-name-transforms
      `((".*" ,(concat user-emacs-directory "var/recovery/") t)))                         ; Set auto-save location  
(setq auto-save-list-file-prefix (concat user-emacs-directory "var/auto-save/sessions/")) ; Set auto-save-list location
(setq load-prefer-newer t)                                                                ; Prefer newer files to load


(setq package-enable-at-startup nil)                                                       
(setq package-quickstart nil)                                                             ; Disable package quickstart

(setq inhibit-startup-screen t)

(setq inhibit-compacting-font-caches t)                                                   ; Don't compact fonts

(set-language-environment "UTF-8")                                                        ; Use UTF-8
(setq-default buffer-file-coding-system 'utf-8-unix)

(setq custom-file (concat user-emacs-directory "etc/custom.el"))                          ; Set custom file location, don't want clutter in main directory
(setq custom-theme-directory
      (concat user-emacs-directory "/etc/materus/themes" ))                               ; Set custom themes location

(setq ring-bell-function 'ignore)                                                         ; Disable bell


(defvar materus/emacs-gc-cons-threshold (* 64 1024 1024)
  "The value of `gc-cons-threshold' after Emacs startup.")                                ; Define after init garbage collector threshold


;;; GARBAGE COLLECTOR 
(setq gc-cons-threshold most-positive-fixnum)                                             ; Set `gc-cons-threshold' so it won't collectect during initialization 

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold materus/emacs-gc-cons-threshold)))                    ; Set `gc-cons-threshold' to desired value after startup




;;; FRAMES

(setq frame-inhibit-implied-resize t)
(setq frame-resize-pixelwise t)
(setq window-resize-pixelwise t)                                ; Allow pixelwise resizing of window and frame

(unless (daemonp)
  (add-to-list 'initial-frame-alist '(fullscreen . maximized))) ; Start first frame maximized if not running as daemon, daemon frame are set up later in config
(setq default-frame-alist                                       ; Set default size for frames
      '((width . 130)   
        (height . 40)))                 

(advice-add #'tty-run-terminal-initialization :override #'ignore)
(add-hook 'window-setup-hook
          (lambda ()
            (unless (display-graphic-p)
              (advice-remove #'tty-run-terminal-initialization #'ignore) 
              (tty-run-terminal-initialization (selected-frame) nil t)
              )))


;;; NATIVE COMPILATION

(setq native-comp-async-report-warnings-errors nil) ; Silence warnings
(setq native-comp-speed 3)                          ; Set native-comp speed

(setq native-comp-jit-compilation t
      ;;native-comp-deferred-compilation t 
      package-native-compile t)


;; Setting up native-comp cache location

(when (and (fboundp 'startup-redirect-eln-cache)
           (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (startup-redirect-eln-cache
   (convert-standard-filename
    (concat user-emacs-directory "var/eln-cache/"))))
