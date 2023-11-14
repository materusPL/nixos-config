
(setq native-comp-async-report-warnings-errors nil)
(setq package-enable-at-startup nil)
(setq frame-inhibit-implied-resize t)


(require 'telephone-line)
(require 'elcord)
(require 'dashboard)
(require 'minions)
(require 'doom-themes)


;Graphical
(when (display-graphic-p)
  (set-frame-font "FiraCode Nerd Font" nil t)
  
)




(setq custom-file (concat user-emacs-directory "custom.el"))
(when (or (not (display-graphic-p)) (daemonp))
  (xterm-mouse-mode 1)
)


(tool-bar-mode -1)
(load-theme 'doom-horizon t)

(setq ring-bell-function 'ignore)
(setq-default cursor-type '(bar . 1))
(pixel-scroll-precision-mode 1)

(setq mouse-wheel-follow-mouse 't)
(setq scroll-step 1)
(setq mouse-drag-and-drop-region t)
(telephone-line-mode 1)
(setq-default pixel-scroll-precision-large-scroll-height 10.0)
(minions-mode 1)
(elcord-mode)

;Hide startup screen if started with file
(defun startup-screen-advice (orig-fun &rest args)
  (when (= (seq-count #'buffer-file-name (buffer-list)) 0)
    (apply orig-fun args)))
(advice-add 'display-startup-screen :around #'startup-screen-advice)





;Enable dashboard
(setq dashboard-center-content t)
(dashboard-setup-startup-hook)
(when (daemonp)
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*"))) ; Show dashboard when emacs is running as daemon
)




;CUA
(cua-mode 1)
(global-set-key (kbd "C-y") 'undo-redo)

;(define-key key-translation-map [8711] 'event-apply-hyper-modifier )
;(global-set-key (kbd "C-∇") (kbd "C-H"))
;(global-set-key (kbd "H-∇") (lambda () (interactive) (insert-char #x2207)))
