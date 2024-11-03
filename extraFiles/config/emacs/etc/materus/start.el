(defvar materus/cfg nil)
(setq materus/cfg (concat user-emacs-directory "etc/materus"))

; Load packages
(require 'telephone-line)
(require 'elcord)
(require 'dashboard)
(require 'minions)
(require 'dracula-theme)
(require 'nerd-icons)
(require 'projectile)
(require 'treemacs)
(require 'treemacs-projectile)
(require 'treemacs-nerd-icons)
(require 'vertico)
(require 'orderless)
(require 'marginalia)
(require 'undo-tree)
(require 'consult)

(cua-mode 0)

;Keybinds
(keymap-set cua--cua-keys-keymap "C-z" 'undo-tree-undo)
(keymap-set cua--cua-keys-keymap "C-y" 'undo-tree-redo)




(keymap-set global-map "TAB" #'indent-rigidly-right-to-tab-stop) 
(keymap-set global-map "<backtab>" #'indent-rigidly-left-to-tab-stop)
(keymap-set global-map "C-<tab>" #'indent-for-tab-commandn)
(keymap-set vertico-map "TAB" #'vertico-insert)

(define-key key-translation-map (kbd "<XF86Calculator>") 'event-apply-hyper-modifier )
(define-key key-translation-map (kbd "<Calculator>") 'event-apply-hyper-modifier )
(define-key key-translation-map (kbd "∇") 'event-apply-hyper-modifier )

(global-set-key (kbd "C-H-t") 'treemacs)

(setq completion-in-region-function
      (lambda (&rest args)
        (apply (if vertico-mode
                   #'consult-completion-in-region
                 #'completion--in-region)
               args)))


(tool-bar-mode -1)
(if (daemonp) 
	    (add-hook 'after-make-frame-functions 
		      (lambda (frame) 
			(with-selected-frame frame (load-theme 'dracula t)))) 
	  (load-theme 'dracula t))


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


(setq-default display-line-numbers-width 4)





(cua-mode 1)


;(global-set-key (kbd "C-∇") (kbd "C-H"))
;(global-set-key (kbd "H-∇") (lambda () (interactive) (insert-char #x2207)))

;; Enable vertico
(vertico-mode 1)
(marginalia-mode 1)

;(setq completion-styles '(orderless basic)
;      completion-category-defaults nil
;      completion-category-overrides '((file (styles partial-completion))))

(electric-pair-mode 1)

(global-undo-tree-mode 1)
(global-tab-line-mode 1)
(setq-default tab-width 4)



(load (concat materus/cfg "/lsp/default"))
