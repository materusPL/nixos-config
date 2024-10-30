(defvar materus/cfg nil)
(setq materus/cfg (concat user-emacs-directory "etc/materus"))

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
(require 'vertico)
(require 'orderless)
(require 'marginalia)








(tool-bar-mode -1)
(if (daemonp) 
	    (add-hook 'after-make-frame-functions 
		      (lambda (frame) 
			(with-selected-frame frame (load-theme 'doom-horizon t)))) 
	  (load-theme 'doom-horizon t))


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

;; Enable vertico
(vertico-mode 1)
(marginalia-mode 1)


;(setq completion-styles '(orderless basic)
;      completion-category-defaults nil
;      completion-category-overrides '((file (styles partial-completion))))

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


(load (concat materus/cfg "/lsp/default"))
