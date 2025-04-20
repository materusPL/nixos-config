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

;; [[file:../../emacs-materus-config.org::*Misc][Misc:1]]
;; Rainbow mode
;; Misc:1 ends here

;; [[file:../../emacs-materus-config.org::*Org-mode][Org-mode:1]]

;; Org-mode:1 ends here

;; [[file:../../emacs-materus-config.org::*LSP][LSP:1]]
(use-package lsp-mode
  :custom
  (lsp-completion-provider :none) ;; we use Corfu!
  :config
  (setq lsp-keep-workspace-alive nil)
  (require 'lsp-ui)
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

;; [[file:../../emacs-materus-config.org::*DAP][DAP:1]]
(use-package dap-mode
  :config
  (require 'dap-lldb)
  (require 'dap-gdb-lldb)
  (require 'dap-cpptools)
  (setq dap-gdb-lldb-extension-version "0.27.0")
  (dap-auto-configure-mode 1)
  )
;; DAP:1 ends here

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

(add-hook 'nix-ts-mode-hook 'lsp-deferred)
(add-hook 'nix-ts-mode-hook 'display-line-numbers-mode)

(when (treesit-language-available-p 'nix) (push '(nix-mode . nix-ts-mode) major-mode-remap-alist))
;; Nix:1 ends here

;; [[file:../../emacs-materus-config.org::*Emacs Lisp][Emacs Lisp:1]]
(add-hook 'emacs-lisp-mode-hook 'display-line-numbers-mode)
;; Emacs Lisp:1 ends here

;; [[file:../../emacs-materus-config.org::*C/C++][C/C++:1]]
(use-package lsp-clangd)
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
(add-to-list 'c-default-style '(c++-ts-mode . "bsd"))
;; C/C++:1 ends here

;; [[file:../../emacs-materus-config.org::*Python][Python:1]]
(use-package lsp-pyright)
(setq lsp-pyright-langserver-command "pyright")
(add-hook 'python-mode-hook 'lsp-deferred)
(add-hook 'python-ts-mode-hook 'lsp-deferred)
(when (treesit-language-available-p 'python) (push '(python-mode . python-ts-mode) major-mode-remap-alist))
;; Python:1 ends here

;; [[file:../../emacs-materus-config.org::*Java][Java:1]]
(use-package lsp-java)
(setq lsp-java-vmargs '("-XX:+UseParallelGC" "-XX:GCTimeRatio=4" "-XX:AdaptiveSizePolicyWeight=90" "-Dsun.zip.disableMemoryMapping=true" "-Xmx2G" "-Xms100m"))
(add-hook 'java-mode-hook (lambda ()  (when (getenv "JDTLS_PATH") (setq lsp-java-server-install-dir (getenv "JDTLS_PATH")))))
(add-hook 'java-mode-hook 'lsp-deferred)
(add-hook 'java-mode-hook 'display-line-numbers-mode)

(add-hook 'java-ts-mode-hook (lambda ()  (when (getenv "JDTLS_PATH") (setq lsp-java-server-install-dir (getenv "JDTLS_PATH")))))
(add-hook 'java-ts-mode-hook 'lsp-deferred)
(add-hook 'java-ts-mode-hook 'display-line-numbers-mode)

(when (treesit-language-available-p 'java) (push '(java-mode . java-ts-mode) major-mode-remap-alist))

(add-to-list 'c-default-style '(java-mode . "java"))
(add-to-list 'c-default-style '(java-ts-mode . "java"))
;; Java:1 ends here

;; [[file:../../emacs-materus-config.org::*Other][Other:1]]
(add-to-list 'c-default-style '(awk-mode . "awk"))
(add-to-list 'c-default-style '(other . "bsd"))




(setq-default c-basic-offset 4)
(setq-default c-indent-level 4)

(setq-default c-ts-mode-indent-offset 4)
(setq-default c-ts-mode-indent-style 'bsd)

(setq-default c-hungry-delete-key t)

(electric-pair-mode 1)
(electric-indent-mode -1)
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'electric-indent-local-mode)
;; Other:1 ends here

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

;;; (buffer-local-value 'var (get-buffer  "your-buffer-name"))

;;; (setq completion-styles '(orderless basic)
;;;   completion-category-defaults nil
;;;   completion-category-overrides '((file (styles partial-completion))))
;; Test:1 ends here
