;Graphical
(when (display-graphic-p)
    (setq-default cursor-type '(bar . 1))
    (set-frame-font "FiraCode Nerd Font" nil t)
)

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

