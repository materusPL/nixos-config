;;; -*- lexical-binding: t; -*-
;; Prepare
(require 'materus-nix nil t)
(require 'elec-pair)
(when (not emacs-build-time)
  (print "WARN: emacs-build-time not set up, using current time")
  (setq emacs-build-time (decode-time (current-time))))
(add-to-list 'load-path (concat user-emacs-directory "etc/pkgs/"))                ; Extra load path for packages  
(setq read-process-output-max (* 1024 1024 3))

;; Elpaca Init
(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "var/elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name (concat "builds/" emacs-version "/") elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
    		      ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(elpaca elpaca-use-package
  (elpaca-use-package-mode)
  (setq elpaca-use-package-by-default t))

(defvar materus/nixos-config (getenv "MATERUS_CONFIG_DIR"))
(defvar materus/server-env nil)
(defvar materus/pkgs/vterm-enable nil)

(let ((vars-file (expand-file-name "etc/variables.el" user-emacs-directory)))
  (unless (file-exists-p vars-file) (make-empty-file vars-file))
  (load vars-file))
;; Use package preffering built-in / nix packages
(defmacro materus/use-package (package &rest body)
  (if (locate-library (symbol-name `,package))
      `(progn
         (cl-pushnew (quote ,package) elpaca-ignored-dependencies)
         (use-package ,package :ensure nil ,@body))
    `(use-package ,package ,@body)))

(defun materus/--outli-modes ()
  "Check if supported mode"
  (or (eq major-mode 'nix-mode)
      (eq major-mode 'nix-ts-mode)
      (eq major-mode 'c-mode)
      (eq major-mode 'c-ts-mode)
      (eq major-mode 'c++-mode)
      (eq major-mode 'c++-ts-mode)))

(defun materus/anchor-outli-headers ()
  "Remove whitespaces before outli headers"
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (while (re-search-forward (concat "^[ 	]+\\(" comment-start "\\*+ +[^ ].*\\)[ 	]*") nil t)
        (replace-match "\\1")))))
(defun materus/--fix-outli-formatting (FORMATTER STATUS)
  (materus/anchor-outli-headers)
  )

(defun materus/--electric-indent-ignore-outli (char)
  "Don't indent outli headers"
  (when (materus/--outli-modes)
    (save-excursion
      (backward-char)
      (beginning-of-line)
      (if (not (looking-at-p  (concat "^\\(" comment-start "\\*+ +[^ ].*\\)[ 	]*"))) nil 'no-indent))))
(use-package no-littering
  :ensure (:wait t)
  :config
  (require 'recentf)
  (setq package-quickstart-file  
        (concat user-emacs-directory "var/quickstart/package-quickstart-" emacs-version ".el" ))
  (add-to-list 'recentf-exclude
               (recentf-expand-file-name no-littering-var-directory))
  (add-to-list 'recentf-exclude
               (recentf-expand-file-name no-littering-etc-directory)))

;; Font & Text init
(when (display-graphic-p)
  (set-frame-font "Hack Nerd Font" nil t))
(setq-default cursor-type '(bar . 2))
(setq truncate-string-ellipsis "…")


(setq text-mode-ispell-word-completion nil) ; Disable ispell
(global-completion-preview-mode 1)
(electric-pair-mode 1)
(electric-indent-mode -1)

(setq isearch-allow-scroll t) ; Allows scrolling without closing isearch
;; Frame Init
(when (daemonp)
  (add-hook 'after-make-frame-functions 
            (lambda (frame) (when (= (length (frame-list)) 2)
                              (set-frame-parameter frame 'fullscreen 'maximized)) 
              (select-frame-set-input-focus frame) )))
(global-tab-line-mode 1)
(setq tab-line-close-tab-function 'kill-buffer)

(setq window-divider-default-bottom-width 1)
(setq window-divider-default-right-width 1)
(window-divider-mode 1)
(tool-bar-mode -1)
(setq-default display-line-numbers-width 3)
(setq-default display-line-numbers-widen t)

 (defun startup-screen-advice (orig-fun &rest args)
  (when (= (seq-count #'buffer-file-name (buffer-list)) 0)
    (apply orig-fun args)))
(advice-add 'display-startup-screen :around #'startup-screen-advice) ; Hide startup screen if started with file
;; Mouse Init
(context-menu-mode 1)
(setq mouse-wheel-follow-mouse 't)
(setq scroll-step 1)
(setq mouse-drag-and-drop-region t)
(xterm-mouse-mode 1)
(pixel-scroll-precision-mode 1)
(setq-default pixel-scroll-precision-large-scroll-height 10.0)
(add-to-list 'c-default-style '(awk-mode . "awk"))
(add-to-list 'c-default-style '(other . "bsd"))

(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset 4)
(setq-default c-indent-level 4)

(setq-default c-ts-mode-indent-offset 4)
(setq-default c-ts-mode-indent-style 'bsd)

(setq-default c-hungry-delete-key t)



(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'electric-indent-local-mode)
(add-hook 'electric-indent-functions 'materus/--electric-indent-ignore-outli)
(use-package dracula-theme :config
  (if (daemonp)
      (add-hook 'after-make-frame-functions
                (lambda (frame)
                  (with-selected-frame frame (load-theme 'dracula t))))
    (load-theme 'dracula t)))

(use-package rainbow-mode
  :hook
  ((org-mode . rainbow-mode)
   (prog-mode . rainbow-mode)))
(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode)
  :config
  (set-face-attribute 'rainbow-delimiters-depth-1-face nil :foreground "#FFFFFF")
  (set-face-attribute 'rainbow-delimiters-depth-2-face nil :foreground "#FFFF00")
  (set-face-attribute 'rainbow-delimiters-depth-5-face nil :foreground "#6A5ACD")
  (set-face-attribute 'rainbow-delimiters-unmatched-face nil :foreground "#FF0000"))

(use-package doom-modeline
  :init (setq doom-modeline-support-imenu t)
  :hook (elpaca-after-init . doom-modeline-mode)
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
  :hook (elpaca-after-init . minions-mode))
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
  (when (or (daemonp) (< (length command-line-args) 2))
    (add-hook 'elpaca-after-init-hook #'dashboard-insert-startupify-lists)
    (add-hook 'elpaca-after-init-hook #'dashboard-initialize)
    (dashboard-setup-startup-hook)))
(when (daemonp)
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))) ; Show dashboard when emacs is running as daemon)
(use-package highlight-indent-guides
  :hook ((prog-mode . highlight-indent-guides-mode)))
(use-package outli
  :ensure (:host github :repo "jdtsmith/outli")
  :hook ((prog-mode . outli-mode)))
(use-package visual-replace
  :defer t
  :bind (("C-r" . visual-replace)
         :map isearch-mode-map
         ("C-r" . visual-replace-from-isearch)))
(use-package eat)

(when (or materus/pkgs/vterm-enable (locate-library (symbol-name 'vterm)))
(materus/use-package vterm))
(use-package orderless
  :init
  ;; Tune the global completion style settings to your liking!
  ;; This affects the minibuffer and non-lsp completion at point.
  (setq completion-styles '(basic partial-completion orderless)
        completion-category-defaults nil
        completion-category-overrides nil))
(use-package consult)
(use-package marginalia)
(use-package embark)
(use-package embark-consult
  :after (embark consult))

(use-package vertico
  :ensure t
  :after (consult marginalia embark)
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
  (vertico-mouse-mode 1)
  :ensure nil
  :after (vertico))
(use-package cape)

(use-package corfu
  :ensure t
  :after (lsp-mode cape)
  ;; Optional customizations
  :custom
  (corfu-cycle nil)                 ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                    ;; Enable auto completion
  (global-corfu-minibuffer nil)
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  (corfu-preview-current nil)       ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches

  ;; Enable Corfu only for certain modes. See also `global-corfu-modes'.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  :init
  (global-corfu-mode 1)
  (corfu-popupinfo-mode 1)
  (corfu-history-mode 1)

  (defun materus/orderless-dispatch-flex-first (_pattern index _total)
    (and (eq index 0) 'orderless-flex))

  (defun materus/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless))
    ;; Optionally configure the first word as flex filtered.
    (add-hook 'orderless-style-dispatchers #'materus/orderless-dispatch-flex-first nil 'local)
    ;; Optionally configure the cape-capf-buster.
    (setq-local completion-at-point-functions (list (cape-capf-buster #'lsp-completion-at-point))))

  :hook
  (lsp-completion-mode . materus/lsp-mode-setup-completion))



(use-package corfu-terminal
  :after (corfu)
  :config
  (when (or (daemonp) (not (display-graphic-p)))
    (corfu-terminal-mode)))

(use-package corfu-mouse
   :after (corfu)
   :ensure (:type git :repo "https://codeberg.org/materus/emacs-corfu-mouse.git")
   :config
   (corfu-mouse-mode)
   (keymap-set corfu--mouse-ignore-map "<mouse-movement>" 'ignore)
   (keymap-set corfu-map "<mouse-movement>" 'ignore))

(use-package kind-icon
  :after (corfu)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))



(use-package dirvish
  :after (nerd-icons)
  :config
  (setq dired-mouse-drag-files t)
  (dirvish-override-dired-mode 1)
  (setq dirvish-attributes
        '(vc-state
          subtree-state
          nerd-icons
          collapse
          git-msg
          file-time 
          file-size))
  )

(use-package treemacs)
(use-package treemacs-projectile
  :after (projectile treemacs))
(use-package treemacs-nerd-icons
  :after (nerd-icons treemacs))
(use-package treemacs-perspective
  :after (treemacs))
(use-package treemacs-mouse-interface
  :after (treemacs)
  :ensure nil)
(use-package lsp-ui
  :after (lsp-mode))
(use-package lsp-mode
  ;; :custom
  ;; (lsp-completion-provider :none) ;; we use Corfu!
  :config
  (setq lsp-keep-workspace-alive nil)
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-ui-doc-show-with-mouse nil)
  
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
      (if (and (not test?)                                                             ; for check lsp-server-present?
               (not (file-remote-p default-directory))                                 ; see lsp-resolve-final-command, it would add extra shell wrapper
               lsp-use-plists
               (not (functionp 'json-rpc-connection))                                  ; native json-rpc
               (executable-find "emacs-lsp-booster"))
          (progn
            (when-let* ((command-from-exec-path (executable-find (car orig-result))))  ; resolve command from exec-path (in case not found in $PATH)
              (setcar orig-result command-from-exec-path))
            (message "Using emacs-lsp-booster for %s!" orig-result)
            (cons "emacs-lsp-booster" orig-result))
        orig-result)))
  (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command))
(use-package lsp-treemacs
  :after (lsp-mode treemacs)
  :config
  (lsp-treemacs-sync-mode 0))


(use-package dap-mode
  :after (lsp-mode)
  :config
  (require 'dap-lldb)
  (require 'dap-gdb-lldb)
  (require 'dap-cpptools)
  (setq dap-gdb-lldb-extension-version "0.27.0")
  (setq dap-auto-configure-features '(sessions locals breakpoints controls))
  (dap-auto-configure-mode 1))

(use-package format-all
  :hook ((prog-mode . format-all-mode))
  :config
  (defun format-all--buffer-from-hook () nil) ; I don't want formatting on save
  (add-hook 'format-all-after-format-functions 'materus/--fix-outli-formatting)
  (setq-default format-all-formatters  
                '(("Nix" (nixfmt))
                  ("C++" (clang-format "--fallback-style=microsoft"))
                  ("C" (clang-format "--fallback-style=microsoft")))))
(use-package yasnippet
  :config
  (yas-global-mode 1))
(use-package lsp-clangd
  :ensure nil
  :after (lsp-mode)
  :config
  (setq lsp-clients-clangd-args '("--fallback-style=microsoft"))

  (add-hook 'c-mode-hook 'lsp-deferred)
  (add-hook 'c-mode-hook 'display-line-numbers-mode)
  (add-hook 'c-ts-mode-hook 'lsp-deferred)
  (add-hook 'c-ts-mode-hook 'display-line-numbers-mode)

  (add-hook 'c++-mode-hook 'lsp-deferred)
  (add-hook 'c++-mode-hook 'display-line-numbers-mode)
  (add-hook 'c++-ts-mode-hook 'lsp-deferred)
  (add-hook 'c++-ts-mode-hook 'display-line-numbers-mode)
  (when (treesit-language-available-p 'c) (push '(c-mode . c-ts-mode) major-mode-remap-alist))
  (when (treesit-language-available-p 'cpp) (push '(c++-mode . c++-ts-mode) major-mode-remap-alist))

  (add-to-list 'c-default-style '(c-mode . "bsd"))
  (add-to-list 'c-default-style '(c++-mode . "bsd"))
  (add-to-list 'c-default-style '(c-ts-mode . "bsd"))
  (add-to-list 'c-default-style '(c++-ts-mode . "bsd")))

(use-package lsp-java
  :after (lsp-mode)
  :config
  (setq lsp-java-vmargs '("-XX:+UseParallelGC" "-XX:GCTimeRatio=4" "-XX:AdaptiveSizePolicyWeight=90" "-Dsun.zip.disableMemoryMapping=true" "-Xmx4G" "-Xms512m"))
  ;;(add-hook 'java-mode-hook (lambda ()  (when (getenv "JDTLS_PATH") (setq lsp-java-server-install-dir (getenv "JDTLS_PATH")))))
  (add-hook 'java-mode-hook #'lsp)
  (add-hook 'java-mode-hook #'display-line-numbers-mode)
  (add-hook 'java-mode-hook #'lsp-lens-mode)
  (add-hook 'java-mode-hook #'lsp-java-boot-lens-mode)

  ;;(add-hook 'java-ts-mode-hook (lambda ()  (when (getenv "JDTLS_PATH") (setq lsp-java-server-install-dir (getenv "JDTLS_PATH")))))
  (add-hook 'java-ts-mode-hook #'lsp)
  (add-hook 'java-ts-mode-hook #'display-line-numbers-mode)
  (add-hook 'java-ts-mode-hook #'lsp-lens-mode)
  (add-hook 'java-ts-mode-hook #'lsp-java-boot-lens-mode)

  (when (treesit-language-available-p 'java) (push '(java-mode . java-ts-mode) major-mode-remap-alist))

  (add-to-list 'c-default-style '(java-mode . "java"))
  (add-to-list 'c-default-style '(java-ts-mode . "java")))
(use-package lsp-pyright
  :after (lsp-mode)
  :if (executable-find "python")
  :config
  (setq lsp-pyright-langserver-command "pyright")
  (add-hook 'python-mode-hook 'lsp-deferred)
  (add-hook 'python-ts-mode-hook 'lsp-deferred)
  (when (treesit-language-available-p 'python) (push '(python-mode . python-ts-mode) major-mode-remap-alist)))
(use-package nix-mode
  :config
  (advice-add 'nix-format-buffer :after #'materus/anchor-outli-headers))
(use-package nix-ts-mode)
(use-package lsp-nix
  :after (lsp-mode nix-mode nix-ts-mode format-all)
  :ensure nil
  :config
  (add-to-list 'lsp-disabled-clients '(nix-mode . nix-nil)) 
  (setq lsp-nix-nixd-server-path "nixd")
  (when (executable-find "nixfmt")  
    (setq lsp-nix-nixd-formatting-command [ "nixfmt" ]))
  
  (unless lsp-nix-nixd-nixos-options-expr
    (setq lsp-nix-nixd-nixos-options-expr (concat "(builtins.getFlake \"/etc/nixos\").nixosConfigurations." (system-name) ".options")))
  (unless lsp-nix-nixd-nixpkgs-expr
    (setq lsp-nix-nixd-nixpkgs-expr (concat "(builtins.getFlake \"/etc/nixos\").nixosConfigurations." (system-name) ".pkgs")))
  (add-hook 'nix-mode-hook 'lsp-deferred)
  (add-hook 'nix-mode-hook 'display-line-numbers-mode)

  (add-hook 'nix-ts-mode-hook 'lsp-deferred)
  (add-hook 'nix-ts-mode-hook 'display-line-numbers-mode)

  (when (treesit-language-available-p 'nix) (push '(nix-mode . nix-ts-mode) major-mode-remap-alist)))
(use-package paredit)

(use-package sly
  :if (executable-find "sbcl")
  :config
  (setq inferior-lisp-program "sbcl"))
(use-package bash-completion)
(use-package diff-hl
  :config
  (setq diff-hl-side 'right)
  (global-diff-hl-mode 1)
  (diff-hl-margin-mode 1)
  (diff-hl-flydiff-mode 1)
  (global-diff-hl-show-hunk-mouse-mode 1))

(use-package magit
  :after (transient))

(use-package git-timemachine
  :defer t)
(use-package org
  :mode (("\\.org$" . org-mode))
  :hook
  ((org-mode . org-indent-mode)
   (org-mode . display-line-numbers-mode)
   )
  :config
  (require 'org-mouse)
  (require 'org-tempo)
  (setq org-src-window-setup 'current-window)
  (setq org-latex-pdf-process '("latexmk -xelatex -quiet -shell-escape -f -output-directory=%o %f"))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((latex . t)
     (emacs-lisp . t)
     (shell . t)
     (css . t)
     (C . t)
     (calc . t)
     (awk . t)
     (sql . t)
     (sqlite . t)))
  (add-hook 'org-mode-hook (lambda ()
                             (setq-local
                              electric-pair-inhibit-predicate
                              `(lambda (c)
                                 (if
                                     (char-equal c ?<) t (,electric-pair-inhibit-predicate c)))))))
(use-package org-modern
  :after (org)
  :hook
  (org-indent-mode . org-modern-mode)
  (org-agenda-finalize . org-modern-agenda)
  :config 
  (setq org-modern-block-name '("▼ " . "▲ ")))
(use-package org-auto-tangle
  :after (org)
  :hook (org-mode . org-auto-tangle-mode)
  )
(use-package toc-org
  :after (org)
  :hook
  ((org-mode . toc-org-mode )
   (markdown-mode . toc-org-mode)))
(use-package org-transclusion
  :after (org))
(use-package org-roam
  :after (org))

(use-package undo-tree
  :config
  (global-undo-tree-mode 1)
  (defvar materus/undo-tree-dir (concat user-emacs-directory "var/undo-tree/"))
  (unless (file-exists-p materus/undo-tree-dir)
    (make-directory materus/undo-tree-dir t))
  (setq undo-tree-visualizer-diff t)
  (setq undo-tree-history-directory-alist `(("." . ,materus/undo-tree-dir )))
  (setq undo-tree-visualizer-timestamps t))

(use-package which-key
    :config
    (which-key-mode 1))
(use-package projectile
  :config (projectile-mode 1))
(use-package  perspective
  :config
  (setq persp-mode-prefix-key (kbd "C-c M-p"))
  (setq persp-modestring-short t)
  (persp-mode 1)
  )

(use-package elcord
  :if (not materus/server-env)
  :config
  (defun materus/elcord-toggle (&optional _frame)
    "Toggle elcord based on visible frames"
    (if (> (length (frame-list)) 1)
        (elcord-mode 1)
      (elcord-mode -1))
    )
  (unless (daemonp) (elcord-mode 1))
  (add-hook 'after-delete-frame-functions 'materus/elcord-toggle)
  (add-hook 'server-after-make-frame-hook 'materus/elcord-toggle))
(use-package drag-stuff)
(use-package popper)
(use-package visual-fill-column)
(use-package so-long
  :defer t)
(use-package vlf
  :defer t)
(use-package crux)

(use-package nerd-icons)
(use-package svg-lib)  
(use-package transient)
;; Keybindings
(defun materus/keybind-set ()
  (require 'cua-base)                               

  ;; CUA-like global
  (define-key global-map (kbd "C-s") 'save-buffer)
  (define-key global-map (kbd "C-a") 'mark-whole-buffer)
  (define-key global-map (kbd "C-f") 'isearch-forward)
  (define-key global-map (kbd "C-S-f") 'isearch-backward)
  (define-key isearch-mode-map (kbd "C-f") 'isearch-repeat-forward)
  (define-key isearch-mode-map (kbd "C-S-f") 'isearch-repeat-backward)

  ;; CUA
  (define-key cua--cua-keys-keymap (kbd "C-z") 'undo-tree-undo)
  (define-key cua--cua-keys-keymap (kbd "C-y") 'undo-tree-redo)
  (define-key cua-global-keymap (kbd "C-SPC") 'completion-at-point)
  (define-key cua-global-keymap (kbd "C-M-SPC") 'cua-set-mark)
  (cua-mode 1)
  ;; TAB
  (define-key global-map (kbd "C-<iso-lefttab>") #'indent-rigidly-left)
  (define-key global-map (kbd "C-<tab>") #'indent-rigidly-right)

  ;; Hyper
  (define-key key-translation-map (kbd "<XF86Calculator>") 'event-apply-hyper-modifier )
  (define-key key-translation-map (kbd "<Calculator>") 'event-apply-hyper-modifier )
  (define-key key-translation-map (kbd "∇") 'event-apply-hyper-modifier )

 
  ;; Unbind
  (define-key isearch-mode-map (kbd "C-s") nil)
  (define-key isearch-mode-map (kbd "C-r") nil)


  ;; Dashboard
  (define-key dashboard-mode-map (kbd "C-r") #'dashboard-refresh-buffer)
  ;; Eat
  (defvar cua--eat-semi-char-keymap (copy-keymap cua--cua-keys-keymap) "EAT semi-char mode CUA keymap")
  (defvar cua--eat-char-keymap (copy-keymap cua--cua-keys-keymap) "EAT char mode CUA keymap")
  
  
  (define-key cua--eat-semi-char-keymap (kbd "C-v") #'eat-yank)
  (define-key cua--eat-char-keymap (kbd "C-S-v") #'eat-yank)
  (define-key cua--eat-semi-char-keymap (kbd "C-c") #'copy-region-as-kill)
  (define-key cua--eat-char-keymap (kbd "C-S-c") #'copy-region-as-kill)
  (define-key eat-mode-map (kbd "C-<right>") #'eat-self-input)
  (define-key eat-mode-map (kbd "C-<left>") #'eat-self-input)
  
  
  (defun cua--eat-semi-char-override-keymap ()
    (setq-local cua--keymap-alist (copy-tree cua--keymap-alist))
    (setf (alist-get 'cua--ena-cua-keys-keymap cua--keymap-alist) cua--eat-semi-char-keymap))
  (defun cua--eat-char-override-keymap ()
    (setq-local cua--keymap-alist (copy-tree cua--keymap-alist))
    (setf (alist-get 'cua--ena-cua-keys-keymap cua--keymap-alist) cua--eat-char-keymap))
  
  (advice-add 'eat-semi-char-mode :after #'cua--eat-semi-char-override-keymap)
  (advice-add 'eat-char-mode :after #'cua--eat-char-override-keymap)
  (add-hook 'eat-char-mode-hook #'cua--eat-char-override-keymap)
  
  ;; Treemacs
  (define-key global-map (kbd "C-H-t") 'treemacs))


(add-hook 'elpaca-after-init-hook #'materus/keybind-set )
;; Custom File
(add-hook 'elpaca-after-init-hook (lambda () (load custom-file 'noerror)))
