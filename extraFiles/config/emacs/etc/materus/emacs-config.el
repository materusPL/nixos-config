;;; -*- lexical-binding: t; -*-

;; [[file:../../emacs-materus-config.org::*Compile Time][Compile Time:2]]
(eval-when-compile 
  (defvar doom-modeline-support-imenu nil)
  (defvar display-time-24hr-format nil)
  (defvar lsp-nix-nixd-formatting-command nil)
  (defvar cua--cua-keys-keymap nil)
  (declare-function lsp-stdio-connection "lsp-mode" (COMMAND &optional TEST-COMMAND))
  (declare-function make-lsp-client "lsp-mode")
  (declare-function lsp-register-client "lsp-mode" ( CLIENT ))
  )
;; Compile Time:2 ends here

;; [[file:../../emacs-materus-config.org::*Init package manager config][Init package manager config:1]]

;; Init package manager config:1 ends here

;; [[file:../../emacs-materus-config.org::*Packages list & function][Packages list & function:1]]
(defvar materus/packages
  '(
    use-package
    elcord
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
    perspective
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
    nixfmt
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
    shfmt
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
        (package-install p)))))
(unless materus/use-nix-packages 
  (materus/install-packages))
;; Packages list & function:1 ends here

;; [[file:../../emacs-materus-config.org::*No Littering][No Littering:1]]
(require 'recentf)
(use-package no-littering
  :config
  (setq package-quickstart-file  
        (concat user-emacs-directory "var/quickstart/package-quickstart-" emacs-version ".el" ))
  (add-to-list 'recentf-exclude
               (recentf-expand-file-name no-littering-var-directory))
  (add-to-list 'recentf-exclude
               (recentf-expand-file-name no-littering-etc-directory)))
;; No Littering:1 ends here

;; [[file:../../emacs-materus-config.org::*Mouse][Mouse:1]]
(context-menu-mode 1)
(setq mouse-wheel-follow-mouse 't)
(setq scroll-step 1)
(setq mouse-drag-and-drop-region t)
(xterm-mouse-mode 1)
(pixel-scroll-precision-mode 1)
(setq-default pixel-scroll-precision-large-scroll-height 10.0)
;; Mouse:1 ends here

;; [[file:../../emacs-materus-config.org::*Misc][Misc:1]]
(when (daemonp)
  (add-hook 'after-make-frame-functions 
            (lambda (frame) (when (= (length (frame-list)) 2)
                              (set-frame-parameter frame 'fullscreen 'maximized)))))


(when (display-graphic-p)
  (set-frame-font "Hack Nerd Font" nil t)
  )

(setq-default display-line-numbers-width 3)
(setq-default display-line-numbers-widen t)
(setq truncate-string-ellipsis "…")

(global-tab-line-mode 1)

(tool-bar-mode -1)
(setq window-divider-default-bottom-width 1)
(setq window-divider-default-right-width 1)
(window-divider-mode 1)

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
  (set-face-attribute 'rainbow-delimiters-depth-1-face nil :foreground "#FFFFFF")
  (set-face-attribute 'rainbow-delimiters-depth-2-face nil :foreground "#FFFF00")
  (set-face-attribute 'rainbow-delimiters-depth-5-face nil :foreground "#6A5ACD")
  (set-face-attribute 'rainbow-delimiters-unmatched-face nil :foreground "#FF0000"))
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
;; Misc:1 ends here

;; [[file:../../emacs-materus-config.org::*Dashboard][Dashboard:1]]
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
;; Dashboard:1 ends here

;; [[file:../../emacs-materus-config.org::*Modeline][Modeline:1]]
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
;; Modeline:1 ends here

;; [[file:../../emacs-materus-config.org::*Diff-hl][Diff-hl:1]]
(use-package diff-hl
  :config
  (setq diff-hl-side 'right)
  (global-diff-hl-mode 1)
  (diff-hl-margin-mode 1)
  (diff-hl-flydiff-mode 1)
  (global-diff-hl-show-hunk-mouse-mode 1))
;; Diff-hl:1 ends here

;; [[file:../../emacs-materus-config.org::*Org-mode][Org-mode:1]]
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
;; Org-mode:1 ends here

;; [[file:../../emacs-materus-config.org::*Style][Style:1]]
(use-package orderless
 :init
 ;; Tune the global completion style settings to your liking!
 ;; This affects the minibuffer and non-lsp completion at point.
 (setq completion-styles '(basic partial-completion orderless)
       completion-category-defaults nil
       completion-category-overrides nil))
;; Style:1 ends here

;; [[file:../../emacs-materus-config.org::*Minibuffer][Minibuffer:1]]
(use-package consult)
(use-package marginalia)

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
;; Minibuffer:1 ends here

;; [[file:../../emacs-materus-config.org::*Code completion][Code completion:1]]
(use-package cape)

(use-package corfu
  ;; Optional customizations
  :custom
  (corfu-cycle nil)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (global-corfu-minibuffer nil)
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  (corfu-preview-current nil)    ;; Disable current candidate preview
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
  (corfu-history-mode 1))


(use-package corfu-terminal
  :after (corfu)
  :config
  (when (or (daemonp) (not (display-graphic-p)))
    (corfu-terminal-mode)))

(use-package corfu-mouse
  :after (corfu)
  :config 
  (corfu-mouse-mode)
  (keymap-set corfu--mouse-ignore-map "<mouse-movement>" 'ignore)
  (keymap-set corfu-map "<mouse-movement>" 'ignore))

(use-package kind-icon
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))
;; Code completion:1 ends here

;; [[file:../../emacs-materus-config.org::*Eat][Eat:1]]
(use-package eat)
;; Eat:1 ends here

;; [[file:../../emacs-materus-config.org::*Defaults][Defaults:1]]
(setq-default buffer-file-coding-system 'utf-8-unix)
(setq text-mode-ispell-word-completion nil) ; Disable ispell
;; Defaults:1 ends here

;; [[file:../../emacs-materus-config.org::*Elcord][Elcord:1]]
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
;; Elcord:1 ends here

;; [[file:../../emacs-materus-config.org::*Undo-Tree][Undo-Tree:1]]
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
;; Undo-Tree:1 ends here

;; [[file:../../emacs-materus-config.org::*Projectile][Projectile:1]]
(use-package projectile
  :config (projectile-mode 1))
;; Projectile:1 ends here

;; [[file:../../emacs-materus-config.org::*Treemacs][Treemacs:1]]
(use-package treemacs)
(use-package treemacs-projectile
  :after (projectile treemacs))
(use-package treemacs-nerd-icons
  :after (nerd-icons treemacs))
(use-package treemacs-perspective
  :after (treemacs))
(use-package treemacs-mouse-interface
  :after (treemacs))
;; Treemacs:1 ends here

;; [[file:../../emacs-materus-config.org::*Magit][Magit:1]]
(use-package magit)
;; Magit:1 ends here

;; [[file:../../emacs-materus-config.org::*Dirvish][Dirvish:1]]
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
;; Dirvish:1 ends here

;; [[file:../../emacs-materus-config.org::*Perspective][Perspective:1]]
(require 'perspective)
(customize-set-variable 'persp-mode-prefix-key (kbd "C-c M-p"))
(persp-mode 1)
;; Perspective:1 ends here

;; [[file:../../emacs-materus-config.org::*LSP][LSP:1]]
(use-package lsp-mode
  :custom
  (lsp-completion-provider :none) ;; we use Corfu!

  :init
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


  (use-package lsp-ui)
  (use-package dap-mode)
  (use-package dap-lldb)
  (use-package dap-gdb-lldb)


  (setq read-process-output-max (* 1024 1024 3))

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
  (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)
;; LSP:1 ends here

;; [[file:../../emacs-materus-config.org::*Nix][Nix:1]]
(use-package nix-mode)
(use-package nix-ts-mode)
(use-package nixfmt)
(use-package lsp-nix)
(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-disabled-clients '(nix-mode . nix-nil)) 
  (setq lsp-nix-nixd-server-path "nixd"
        lsp-nix-nixd-formatting-command [ "nixfmt" ]
        lsp-nix-nixd-nixpkgs-expr "import <nixpkgs> { }"))

(setq lsp-nix-nixd-formatting-command "nixfmt")
(add-hook 'nix-mode-hook 'lsp-deferred)
(add-hook 'nix-mode-hook 'display-line-numbers-mode)

;;(add-hook 'nix-ts-mode-hook 'lsp-deferred)
;;(add-hook 'nix-ts-mode-hook 'display-line-numbers-mode)

;;(when (treesit-language-available-p 'nix) (push '(nix-mode . nix-ts-mode) major-mode-remap-alist))
;; Nix:1 ends here

;; [[file:../../emacs-materus-config.org::*Emacs Lisp][Emacs Lisp:1]]
(add-hook 'emacs-lisp-mode-hook 'display-line-numbers-mode)
;; Emacs Lisp:1 ends here

;; [[file:../../emacs-materus-config.org::*C/C++][C/C++:1]]
(use-package lsp-clangd)
(setq lsp-clients-clangd-args '("--fallback-style=microsoft"))

(add-hook 'c-mode-hook 'lsp-deferred)
(add-hook 'c-mode-hook 'display-line-numbers-mode)
;(add-hook 'c-ts-mode-hook 'lsp-deferred)
;(add-hook 'c-ts-mode-hook 'display-line-numbers-mode)

(add-hook 'c++-mode-hook 'lsp-deferred)
(add-hook 'c++-mode-hook 'display-line-numbers-mode)
;(add-hook 'c++-ts-mode-hook 'lsp-deferred)
;(add-hook 'c++-ts-mode-hook 'display-line-numbers-mode)
;(when (treesit-language-available-p 'c) (push '(c-mode . c-ts-mode) major-mode-remap-alist))
;(when (treesit-language-available-p 'cpp) (push '(c++-mode . c++-ts-mode) major-mode-remap-alist))

(add-to-list 'c-default-style '(c-mode . "bsd"))
(add-to-list 'c-default-style '(c++-mode . "bsd"))
;(add-to-list 'c-default-style '(c-ts-mode . "bsd"))
;(add-to-list 'c-default-style '(c++-ts-mode . "bsd"))
;; C/C++:1 ends here

;; [[file:../../emacs-materus-config.org::*Python][Python:1]]
(use-package lsp-pyright)
(setq lsp-pyright-langserver-command "pyright")
(add-hook 'python-mode-hook 'lsp-deferred)
;; Python:1 ends here

;; [[file:../../emacs-materus-config.org::*Java][Java:1]]
(use-package lsp-java)
(setq lsp-java-vmargs '("-XX:+UseParallelGC" "-XX:GCTimeRatio=4" "-XX:AdaptiveSizePolicyWeight=90" "-Dsun.zip.disableMemoryMapping=true" "-Xmx2G" "-Xms100m"))
(add-hook 'java-mode-hook (lambda ()  (when (getenv "JDTLS_PATH") (setq lsp-java-server-install-dir (getenv "JDTLS_PATH")))))
(add-hook 'java-mode-hook 'lsp-deferred)
(add-hook 'java-mode-hook 'display-line-numbers-mode)

;(add-hook 'java-ts-mode-hook (lambda ()  (when (getenv "JDTLS_PATH") (setq lsp-java-server-install-dir (getenv "JDTLS_PATH")))))
;(add-hook 'java-ts-mode-hook 'lsp-deferred)
;(add-hook 'java-ts-mode-hook 'display-line-numbers-mode)

;(when (treesit-language-available-p 'java) (push '(java-mode . java-ts-mode) major-mode-remap-alist))

(add-to-list 'c-default-style '(java-mode . "java"))
(add-to-list 'c-default-style '(java-ts-mode . "java"))
;; Java:1 ends here

;; [[file:../../emacs-materus-config.org::*Other][Other:1]]
(add-to-list 'c-default-style '(awk-mode . "awk"))
(add-to-list 'c-default-style '(other . "bsd"))




(setq-default c-basic-offset 4)
(setq-default c-indent-level 4)
(setq-default c-hungry-delete-key t)
(electric-pair-mode 1)
(electric-indent-mode -1)
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'electric-indent-local-mode)
;; Other:1 ends here

;; [[file:../../emacs-materus-config.org::*Keybindings][Keybindings:1]]
(use-package cua-base)

  ;;; Keybinds
;; Eat Term
(keymap-set eat-semi-char-mode-map "C-v" #'eat-yank)
(keymap-set eat-char-mode-map "C-V" #'eat-yank)
;; perspective
(global-set-key (kbd "C-x C-b") 'persp-list-buffers)
(global-set-key (kbd "C-x C-B") 'list-buffers)
(global-set-key (kbd "C-x b") 'persp-switch-to-buffer)
(global-set-key (kbd "C-x B") 'consult-buffer)

;; CUA
(keymap-set cua--cua-keys-keymap "C-z" 'undo-tree-undo)
(keymap-set cua--cua-keys-keymap "C-y" 'undo-tree-redo)
(cua-mode 1)
;; TAB
(keymap-set global-map "C-<iso-lefttab>" #'indent-rigidly-left-to-tab-stop)
(keymap-set global-map "C-<tab>" #'indent-rigidly-right-to-tab-stop)
;; Hyper
(define-key key-translation-map (kbd "<XF86Calculator>") 'event-apply-hyper-modifier )
(define-key key-translation-map (kbd "<Calculator>") 'event-apply-hyper-modifier )
(define-key key-translation-map (kbd "∇") 'event-apply-hyper-modifier )

(global-set-key (kbd "C-H-t") 'treemacs)
;; Keybindings:1 ends here

;; [[file:../../emacs-materus-config.org::*Yasnippet init][Yasnippet init:1]]
(use-package yasnippet
:config (yas-global-mode 1))
;; Yasnippet init:1 ends here

;; [[file:../../emacs-materus-config.org::*Update config script][Update config script:1]]
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
;; Update config script:1 ends here

;; [[file:../../emacs-materus-config.org::*Byte compile][Byte compile:1]]
(materus/compile-config-if-needed)
;; Byte compile:1 ends here

;; [[file:../../emacs-materus-config.org::*Test][Test:1]]
;;; (global-set-key (kbd "C-∇") (kbd "C-H"))
;;; (global-set-key (kbd "H-∇") (lambda () (interactive) (insert-char #x2207)))
;;; (buffer-text-pixel-size)
;;; (set-window-vscroll nil 960 t t)

;;;  (set-window-margins (selected-window) 0 0)

;;; (setq completion-styles '(orderless basic)
;;;   completion-category-defaults nil
;;;   completion-category-overrides '((file (styles partial-completion))))
;; Test:1 ends here
