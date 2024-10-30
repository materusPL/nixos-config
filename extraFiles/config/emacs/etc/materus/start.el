(require 'recentf)
(require 'no-littering)
(add-to-list 'recentf-exclude
        (recentf-expand-file-name no-littering-var-directory))
(add-to-list 'recentf-exclude
        (recentf-expand-file-name no-littering-etc-directory))

(setq native-comp-async-report-warnings-errors nil)
(setq package-enable-at-startup nil)
(setq frame-inhibit-implied-resize t)
(defvar materus/init-early nil)
(unless materus/init-early
        (tool-bar-mode -1)
        (setq initial-major-mode 'fundamental-mode)
        (setq-default package-quickstart t)
        (setq native-comp-speed 3)
        (add-hook 'emacs-startup-hook (lambda () (package-initialize)
        (setq gc-cons-threshold 100000000 ; ~100mb
                gc-cons-percentage 0.1)
        ))
        (unless (daemonp)
  (setq gc-cons-threshold most-positive-fixnum ; 2^61 bytes
      gc-cons-percentage 0.6)
  (setq  package-enable-at-startup nil)
  
  (advice-add #'tty-run-terminal-initialization :override #'ignore)
  (add-hook 'window-setup-hook
    (lambda ()
      (advice-remove #'tty-run-terminal-initialization #'ignore)
      (tty-run-terminal-initialization (selected-frame) nil t)
    )
  )
)
)



(setq custom-file (concat user-emacs-directory "etc/custom.el"))

(setq auto-save-default nil)          
(setq backup-directory-alist
  `((".*" . ,(concat user-emacs-directory "var/backups/"))))
(setq auto-save-file-name-transforms
  `((".*" ,(concat user-emacs-directory "var/recovery/") t)))
  

; Load packages
(require 'telephone-line)
(require 'elcord)
(require 'dashboard)
(require 'minions)
(require 'doom-themes)
(require 'nerd-icons)
(require 'centaur-tabs)
(require 'projectile)
(require 'treemacs)
(require 'treemacs-projectile)
(require 'treemacs-nerd-icons)

;Graphical
(setq frame-resize-pixelwise t)
(when (display-graphic-p)
  (set-frame-font "Hack Nerd Font" nil t)
)



(setq read-process-output-max (* 1024 1024 3))
(when (or (not (display-graphic-p)) (daemonp))
  (xterm-mouse-mode 1)
)


(tool-bar-mode -1)
(load-theme 'doom-horizon t)

(setq ring-bell-function 'ignore)
(setq-default cursor-type '(bar . 1))
(pixel-scroll-precision-mode 1)

(context-menu-mode 1)
(setq mouse-wheel-follow-mouse 't)
(setq scroll-step 1)
(setq mouse-drag-and-drop-region t)
(telephone-line-mode 1)
(setq-default pixel-scroll-precision-large-scroll-height 10.0)
(minions-mode 1)
(unless (daemonp)
  (elcord-mode 1))

;Hide startup screen if started with file
(defun startup-screen-advice (orig-fun &rest args)
  (when (= (seq-count #'buffer-file-name (buffer-list)) 0)
    (apply orig-fun args)))
(advice-add 'display-startup-screen :around #'startup-screen-advice)





;Enable dashboard
(setq dashboard-center-content t)
(setq dashboard-display-icons-p t)
(setq dashboard-icon-type 'nerd-icons)
(dashboard-setup-startup-hook)
(when (daemonp)
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*"))) ; Show dashboard when emacs is running as daemon
)

;Enable Centaur tabs
(centaur-tabs-mode t)
(setq centaur-tabs-set-bar 'over)
(setq centaur-tabs-set-modified-marker t)
;(setq centaur-tabs-modified-marker "*")

;Enable treemacs
(global-set-key (kbd "C-H-t") 'treemacs)

;CUA
(cua-mode 1)
(global-set-key (kbd "C-y") 'undo-redo)

(global-set-key (kbd "C-<tab>") 'indent-rigidly-right-to-tab-stop)
(global-set-key (kbd "<backtab>") 'indent-rigidly-left-to-tab-stop)


(define-key key-translation-map (kbd "<XF86Calculator>") 'event-apply-hyper-modifier )
(define-key key-translation-map (kbd "<Calculator>") 'event-apply-hyper-modifier )
(define-key key-translation-map (kbd "∇") 'event-apply-hyper-modifier )

;(global-set-key (kbd "C-∇") (kbd "C-H"))
;(global-set-key (kbd "H-∇") (lambda () (interactive) (insert-char #x2207)))