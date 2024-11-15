;;; -*- lexical-binding: t; -*-
(eval-when-compile 
  (defvar doom-modeline-support-imenu nil)
  (defvar display-time-24hr-format nil)
  (defvar lsp-nix-nixd-formatting-command nil)
  (defvar cua--cua-keys-keymap nil)
  (declare-function lsp-stdio-connection "lsp-mode" (COMMAND &optional TEST-COMMAND))
  (declare-function make-lsp-client "lsp-mode")
  (declare-function lsp-register-client "lsp-mode" ( CLIENT ))
  )

(require 'cl-lib)
(require 'package)
(setq package-user-dir (concat user-emacs-directory "var/elpa/" emacs-version "/" ))
(setq package-gnupghome-dir (concat user-emacs-directory "var/elpa/gnupg/" ))
(setq package-quickstart t)
(setq package-quickstart-file  
      (concat user-emacs-directory "var/quickstart/package-quickstart-" emacs-version ".el" ))
(add-to-list 'load-path (concat user-emacs-directory "etc/materus/extra"))

(add-to-list 'package-archives '("nongnu-devel" . "https://elpa.nongnu.org/nongnu-devel/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(defvar materus/packages
  '(
    use-package
    elcord
    persp-mode
    dashboard
    magit
    git-timemachine
    avy
    vterm
    direnv
    projectile
    clipetty
    which-key
    iedit
    hideshowvis
    evil
    treemacs-evil
    treemacs
    treemacs-nerd-icons
    treemacs-perspective
    treemacs-icons-dired
    treemacs-magit
    treemacs-projectile
    tree-edit
    nerd-icons
    nerd-icons-completion
    perspective
    minions
    rainbow-delimiters
    rainbow-mode
    cmake-mode
    lsp-mode
    lsp-java
    lsp-jedi
    lsp-haskell
    lsp-ui
    lsp-treemacs
    flycheck
    gradle-mode
    groovy-mode
    kotlin-mode
    dap-mode
    d-mode
    lua-mode
    multiple-cursors
    org
    org-contrib
    org-ql
    org-rainbow-tags
    org-roam
    org-roam-ui
    org-review
    org-present
    org-superstar
    org-auto-tangle
    visual-fill-column
    csharp-mode
    markdown-mode
    json-mode
    nix-mode
    no-littering
    right-click-context
    dracula-theme
    doom-themes
    doom-modeline
    popper
    undo-tree
    bash-completion
    eldoc-box
    yasnippet
    async
    request
    nix-ts-mode
    markdown-ts-mode
    llvm-ts-mode
    treesit-fold
    treesit-auto
    tree-sitter-langs
    eat
    vlf
    edit-indirect
    zones
    sudo-edit
    toc-org
    empv
    volatile-highlights
    highlight
    elfeed
    elfeed-goodies
    drag-stuff
    dirvish
    rg
    ;; Completions & Minibuffer
    corfu
    company
    company-quickhelp
    cape
    embark
    embark-consult
    orderless
    vertico
    marginalia
    )
  "A list of packages to ensure are installed at launch.")

(defun materus/packages-installed-p ()
  (cl-loop for p in materus/packages
           when (not (package-installed-p p)) do (cl-return nil)
           finally (cl-return t)))

(defun materus/install-packages ()
  (unless (materus/packages-installed-p)
    (package-refresh-contents)
    (dolist (p materus/packages)
      (when (not (package-installed-p p))
        (package-install p)))
    (package-quickstart-refresh)))
(unless materus/use-nix-packages 
  (package-initialize)
  (materus/install-packages)
  (unless (file-exists-p package-quickstart-file) (package-quickstart-refresh) ))

(require 'recentf)
(use-package no-littering
:config
(setq package-quickstart-file  
      (concat user-emacs-directory "var/quickstart/package-quickstart-" emacs-version ".el" ))
(add-to-list 'recentf-exclude
             (recentf-expand-file-name no-littering-var-directory))
(add-to-list 'recentf-exclude
             (recentf-expand-file-name no-littering-etc-directory)))

(context-menu-mode 1)
(setq mouse-wheel-follow-mouse 't)
(setq scroll-step 1)
(setq mouse-drag-and-drop-region t)
(xterm-mouse-mode 1)
(pixel-scroll-precision-mode 1)
(setq-default pixel-scroll-precision-large-scroll-height 10.0)

(when (daemonp)
  (add-hook 'after-make-frame-functions 
            (lambda (frame) (when (= (length (frame-list)) 2)
                              (set-frame-parameter frame 'fullscreen 'maximized)))))

(setq frame-inhibit-implied-resize t)
(setq frame-resize-pixelwise t)
(setq window-resize-pixelwise t)
(when (display-graphic-p)
  (set-frame-font "Hack Nerd Font" nil t)
  )

(setq-default display-line-numbers-width 3)


(global-tab-line-mode 1)
(setq-default tab-width 4)
(tool-bar-mode -1)

(setq read-process-output-max (* 1024 1024 3))
(setq ring-bell-function 'ignore)
(setq-default cursor-type '(bar . 1))
;; Rainbow mode
(use-package rainbow-mode
  :hook
  ((org-mode . rainbow-mode)
   (prog-mode . rainbow-mode)))

;; Delimiters
(use-package rainbow-delimiters :hook
  (prog-mode . rainbow-delimiters-mode)
  :config
  (custom-set-faces
   '(rainbow-delimiters-depth-1-face ((t (:foreground "#FFFFFF"))))
   '(rainbow-delimiters-depth-2-face ((t (:foreground "#FFFF00"))))
   '(rainbow-delimiters-depth-5-face ((t (:foreground "#6A5ACD"))))
   '(rainbow-delimiters-unmatched-face ((t (:foreground "#FF0000")))))
  )
;; Nerd Icons
(use-package nerd-icons)
(use-package nerd-icons-completion
  :after (marginalia)
  :config 
  (nerd-icons-completion-mode 1)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

;; Theme
(use-package dracula-theme :config
  (if (daemonp)
      (add-hook 'after-make-frame-functions
                (lambda (frame)
                  (with-selected-frame frame (load-theme 'dracula t))))
    (load-theme 'dracula t)))

(defun startup-screen-advice (orig-fun &rest args)
  (when (= (seq-count #'buffer-file-name (buffer-list)) 0)
    (apply orig-fun args)))
(advice-add 'display-startup-screen :around #'startup-screen-advice) ; Hide startup screen if started with file

(use-package dashboard
  :after (nerd-icons projectile)
  :config
  (setq dashboard-center-content t)
  (setq dashboard-display-icons-p t)
  (setq dashboard-icon-type 'nerd-icons)
  (setq dashboard-projects-backend 'projectile)
  (setq dashboard-items '((recents   . 5)
                          (bookmarks . 5)
                          (projects  . 5)
                          (agenda    . 5)
                          (registers . 5)))
  (dashboard-setup-startup-hook)
  (when (daemonp)
    (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*"))) ; Show dashboard when emacs is running as daemon
    )
  )

(use-package doom-modeline
  :init (setq doom-modeline-support-imenu t)
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-icon t)
  (setq doom-modeline-project-detection 'auto)
  (setq doom-modeline-height 20)
  (setq doom-modeline-enable-word-count t)
  (setq doom-modeline-minor-modes t)
  (setq display-time-24hr-format t)
  (display-time-mode 1)
  (column-number-mode 1)
  (line-number-mode 1))

(use-package minions
  :hook (after-init . minions-mode))

(use-package org
  :mode (("\\.org$" . org-mode))
  :hook
  ((org-mode . org-indent-mode)
   (org-mode . display-line-numbers-mode)
   )
  :config
  (require 'org-mouse)
  (require 'org-tempo)
  (add-hook 'org-mode-hook (lambda ()
							 (setq-local
							  electric-pair-inhibit-predicate
							  `(lambda (c)
								 (if
									 (char-equal c ?<) t (,electric-pair-inhibit-predicate c)))))))

(use-package org-superstar
  :after (org)
  :hook
  (org-mode . org-superstar-mode))
:config
(setq org-superstar-leading-bullet " ")
(use-package org-auto-tangle
  :after (org)
  :hook (org-mode . org-auto-tangle-mode))
(use-package toc-org
  :after (org)
  :hook
  ((org-mode . toc-org-mode )
   (markdown-mode . toc-org-mode)))

(use-package consult)
(use-package marginalia)
(use-package orderless)

(use-package which-key
  :config
  (which-key-mode 1))

(use-package vertico
  :after (consult marginalia)
  :config
  (setq completion-in-region-function
        (lambda (&rest args)
          (apply (if vertico-mode
                     #'consult-completion-in-region
                   #'completion--in-region)
                 args)))
  (vertico-mode 1)
  (marginalia-mode 1))
(use-package vertico-mouse
  :config
  (vertico-mouse-mode 1))

(use-package company
  :config 
  (setq global-corfu-minibuffer nil)
  (global-company-mode 1))

(electric-pair-mode 1)
(electric-indent-mode -1)
(setq-default indent-tabs-mode nil)
(setq-default buffer-file-coding-system 'utf-8-unix)

(defun materus/elcord-toggle (&optional _frame)
  "Toggle elcord based on visible frames"
  (if (> (length (frame-list)) 1)
      (elcord-mode 1)
    (elcord-mode -1))
  )
(use-package elcord
  :config
  (unless (daemonp) (elcord-mode 1))
  (add-hook 'after-delete-frame-functions 'materus/elcord-toggle)
  (add-hook 'server-after-make-frame-hook 'materus/elcord-toggle))

(use-package undo-tree
:config
(global-undo-tree-mode 1)
(defvar materus/undo-tree-dir (concat user-emacs-directory "var/undo-tree/"))
(unless (file-exists-p materus/undo-tree-dir)
    (make-directory materus/undo-tree-dir t))
(setq undo-tree-visualizer-diff t)
(setq undo-tree-history-directory-alist `(("." . ,materus/undo-tree-dir )))
(setq undo-tree-visualizer-timestamps t)
)

(use-package projectile
  :config (projectile-mode 1))

(use-package treemacs)
(use-package treemacs-projectile
:after (projectile treemacs))
(use-package treemacs-nerd-icons
:after (nerd-icons treemacs))

(use-package magit)

(use-package dirvish 
  :config (dirvish-override-dired-mode 1)
  (setq dirvish-attributes
        '(vc-state
          subtree-state
          nerd-icons
          collapse
          git-msg
          file-time 
          file-size)))

(use-package lsp-mode)


(use-package lsp-ui)
(use-package dap-mode)
(use-package dap-lldb)
(use-package dap-gdb-lldb)


(defun lsp-booster--advice-json-parse (old-fn &rest args)
  "Try to parse bytecode instead of json."
  (or
   (when (equal (following-char) ?#)
     (let ((bytecode (read (current-buffer))))
       (when (byte-code-function-p bytecode)
         (funcall bytecode))))
   (apply old-fn args)))
(advice-add (if (progn (require 'json)
                       (fboundp 'json-parse-buffer))
                'json-parse-buffer
              'json-read)
            :around
            #'lsp-booster--advice-json-parse)

(defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
  "Prepend emacs-lsp-booster command to lsp CMD."
  (let ((orig-result (funcall old-fn cmd test?)))
    (if (and (not test?)                             ;; for check lsp-server-present?
             (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
             lsp-use-plists
             (not (functionp 'json-rpc-connection))  ;; native json-rpc
             (executable-find "emacs-lsp-booster"))
        (progn
          (when-let* ((command-from-exec-path (executable-find (car orig-result))))  ;; resolve command from exec-path (in case not found in $PATH)
            (setcar orig-result command-from-exec-path))
          (message "Using emacs-lsp-booster for %s!" orig-result)
          (cons "emacs-lsp-booster" orig-result))
      orig-result)))
(advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)


(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(with-eval-after-load 'lsp-mode
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection "nixd")
                    :major-modes '(nix-mode)
                    :priority 0
                    :server-id 'nixd)))
(setq lsp-nix-nixd-formatting-command "nixfmt")
(add-hook 'nix-mode-hook 'lsp-deferred)
(add-hook 'nix-mode-hook 'display-line-numbers-mode)

(add-hook 'emacs-lisp-mode-hook 'display-line-numbers-mode)

(add-hook 'c-mode-hook 'lsp-deferred)
(add-hook 'c-mode-hook 'display-line-numbers-mode)

(add-hook 'c++-mode-hook 'lsp-deferred)
(add-hook 'c++-mode-hook 'display-line-numbers-mode)

(use-package lsp-java
  :config   
  (add-hook 'java-mode-hook (lambda ()  (when (getenv "JDTLS_PATH") (setq lsp-java-server-install-dir (getenv "JDTLS_PATH")))))
  (add-hook 'java-mode-hook 'lsp-deferred)
  (add-hook 'java-mode-hook 'display-line-numbers-mode))

(use-package cua-base)

;; Keybinds
(keymap-set cua--cua-keys-keymap "C-z" 'undo-tree-undo)
(keymap-set cua--cua-keys-keymap "C-y" 'undo-tree-redo)

(keymap-set global-map "C-<iso-lefttab>" #'indent-rigidly-left-to-tab-stop)
(keymap-set global-map "C-<tab>" #'indent-rigidly-right-to-tab-stop)

(define-key key-translation-map (kbd "<XF86Calculator>") 'event-apply-hyper-modifier )
(define-key key-translation-map (kbd "<Calculator>") 'event-apply-hyper-modifier )
(define-key key-translation-map (kbd "∇") 'event-apply-hyper-modifier )

(global-set-key (kbd "C-H-t") 'treemacs)
(cua-mode 1)

(use-package yasnippet
:config (yas-global-mode 1))

(defun materus/sync-config ()
  "Function to sync config from MATERUS_CONFIG_DIR to emacs folder"
  (if (getenv "MATERUS_CONFIG_DIR")
      (progn (copy-directory (concat (getenv "MATERUS_CONFIG_DIR") "extraFiles/config/emacs/") 
                             user-emacs-directory t t t) t)
    (progn (message "Can't use if MATERUS_CONFIG_DIR is not set!") nil)))
(defun materus/compare-file-time (file1 file2)
  "Returns t when file1 is newer than file2"
  (time-less-p 
   (nth 5 (file-attributes file2))
   (nth 5 (file-attributes file1))
   ))
(defun materus/compile-if-needed (file)
  (unless (and (file-exists-p (concat user-emacs-directory file "c"))
               (materus/compare-file-time (concat user-emacs-directory file "c")
                                          (concat user-emacs-directory file)))
    (byte-compile-file (concat user-emacs-directory file)))
  )
(defun materus/compile-config-if-needed ()
  (materus/compile-if-needed "early-init.el")
  (materus/compile-if-needed "init.el")
  (materus/compile-if-needed "etc/materus/emacs-config.el"))
(defun materus/update-config ()
  "Will sync and compile config"
  (interactive)
  (when (materus/sync-config) (materus/compile-config-if-needed) (byte-recompile-directory (concat user-emacs-directory "etc/materus/extra") 0 t)))

(materus/compile-config-if-needed)

;;; (global-set-key (kbd "C-∇") (kbd "C-H"))
;;; (global-set-key (kbd "H-∇") (lambda () (interactive) (insert-char #x2207)))



;;; (setq completion-styles '(orderless basic)
;;;   completion-category-defaults nil
;;;   completion-category-overrides '((file (styles partial-completion))))
