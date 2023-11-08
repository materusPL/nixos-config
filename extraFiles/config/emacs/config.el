;Graphical
(when (display-graphic-p)
  (set-frame-font "FiraCode Nerd Font" nil t)
  (tool-bar-mode -1)
)

(load-theme 'moe-dark)


(setq-default cursor-type '(bar . 1))
(pixel-scroll-precision-mode 1)
(setq pixel-scroll-precision-large-scroll-height 10.0)
(setq mouse-wheel-follow-mouse 't)
(setq scroll-step 1)
(setq mouse-drag-and-drop-region t)
(telephone-line-mode 1)
(minions-mode 1)

;Hide startup screen if started with file
(defun startup-screen-advice (orig-fun &rest args)
  (when (= (seq-count #'buffer-file-name (buffer-list)) 0)
    (apply orig-fun args)))
(advice-add 'display-startup-screen :around #'startup-screen-advice)





;Enable dashboard
(dashboard-setup-startup-hook)
(when (daemonp)
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*"))) ; Show dashboard when emacs is running as daemon
)




:CUA
(cua-mode 1)
(global-set-key (kbd "C-y") 'undo-redo)

