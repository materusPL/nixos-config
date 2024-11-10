(unless (file-exists-p (concat user-emacs-directory "etc/materus/emacs-config.elc"))
  (byte-compile-file (concat user-emacs-directory "etc/materus/emacs-config.el")))

(unless (file-exists-p (concat user-emacs-directory "init.elc"))
  (byte-compile-file (concat user-emacs-directory "init.el")))

(unless (file-exists-p (concat user-emacs-directory "early-init.elc"))
  (byte-compile-file (concat user-emacs-directory "early-init.el")))

(require 'cl-lib)
(require 'package)
(setq package-user-dir (concat user-emacs-directory "var/elpa/" emacs-version "/" ))
(add-to-list 'package-archives '("nongnu-devel" . "https://elpa.nongnu.org/nongnu-devel/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(defvar materus/packages
  '(
    use-package
    elcord
    persp-mode
    dashboard
    magit
    git-timemachine
    avy
    corfu
    vterm
    projectile
    company
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
    vertico
    marginalia
    nerd-icons
    nerd-icons-completion
    perspective
    minions
    doom-modeline
    rainbow-delimiters
    rainbow-mode
    use-package
    cmake-mode
    lsp-mode
    lsp-java
    lsp-jedi
    lsp-haskell
    lsp-ui
    lsp-treemacs
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
    orderless
    popper
    undo-tree
    bash-completion
    consult
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
    eshell-vterm
    empv
	volatile-highlights
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
		(package-install p)))))
(materus/install-packages)

(require 'recentf)
(use-package no-littering
:config
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

(setq frame-inhibit-implied-resize t)
(setq frame-resize-pixelwise t)
(setq window-resize-pixelwise t)
(when (display-graphic-p)
  (set-frame-font "Hack Nerd Font" nil t)
  )

(setq-default display-line-numbers-width 4)


(global-tab-line-mode 1)
(setq-default tab-width 4)
(tool-bar-mode -1)

(setq read-process-output-max (* 1024 1024 3))
(setq ring-bell-function 'ignore)
(setq-default cursor-type '(bar . 1))


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
:after (nerd-icons)
:config
  (setq dashboard-center-content t)
  (setq dashboard-display-icons-p t)
  (setq dashboard-icon-type 'nerd-icons)
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
  (setq display-time-24hr-format t)
  (display-time-mode 1))

(use-package minions
  :hook (after-init . minions-mode))

(use-package org
  :mode (("\\.org$" . org-mode))
  :hook
  ((org-mode . org-indent-mode)
   (org-mode . (lambda ()
         (setq-local electric-pair-inhibit-predicate
                 `(lambda (c)
                (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c)))))))
  :config
  (require 'org-mouse)
  (require 'org-tempo))
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

(use-package company
:hook (after-init-hook . global-company-mode))

(electric-pair-mode 1)
(electric-indent-mode 0)

(defun materus/elcord-toggle (&optional _frame)
  "Toggle elcord based on visible frames"
  (if (> (length (frame-list)) 1)
      (elcord-mode 1)
    (elcord-mode -1))
  )
(use-package elcord
  :init (unless (daemonp) (elcord-mode 1))
  :config
  (add-hook 'after-delete-frame-functions 'materus/elcord-toggle)
  (add-hook 'server-after-make-frame-hook 'materus/elcord-toggle))

(use-package undo-tree
:init (global-undo-tree-mode 1)
:config
(defvar materus/undo-tree-dir (concat user-emacs-directory "var/undo-tree/"))
(unless (file-exists-p materus/undo-tree-dir)
    (make-directory materus/undo-tree-dir t))
(setq undo-tree-visualizer-diff t)
(setq undo-tree-history-directory-alist `(("." . ,materus/undo-tree-dir )))
(setq undo-tree-visualizer-timestamps t)
)

(use-package projectile)

(use-package treemacs)
(use-package treemacs-projectile
:after (projectile treemacs))
(use-package treemacs-nerd-icons
:after (nerd-icons treemacs))

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
          (when-let ((command-from-exec-path (executable-find (car orig-result))))  ;; resolve command from exec-path (in case not found in $PATH)
            (setcar orig-result command-from-exec-path))
          (message "Using emacs-lsp-booster for %s!" orig-result)
          (cons "emacs-lsp-booster" orig-result))
      orig-result)))
(advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)

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
(add-hook 'emacs-lisp-mode-hook 'company-mode)

(add-hook 'c-mode-hook 'lsp-deferred)
(add-hook 'c-mode-hook 'display-line-numbers-mode)

(add-hook 'c++-mode-hook 'lsp-deferred)
(add-hook 'c++-mode-hook 'display-line-numbers-mode)

(add-hook 'java-mode-hook 'lsp-deferred)

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

;;; (global-set-key (kbd "C-∇") (kbd "C-H"))
;;; (global-set-key (kbd "H-∇") (lambda () (interactive) (insert-char #x2207)))



;;; (setq completion-styles '(orderless basic)
;;;	   completion-category-defaults nil
;;;	   completion-category-overrides '((file (styles partial-completion))))