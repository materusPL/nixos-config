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

;Evil
(cua-mode 0) ; to load cua commmands but not set cua-mode
(evil-mode 1)
(define-key evil-insert-state-map (kbd "C-c") 'cua-copy-region)
(define-key evil-insert-state-map (kbd "C-v") 'cua-paste)
(define-key evil-insert-state-map (kbd "C-x") 'cua-cut-region)
(define-key evil-insert-state-map (kbd "C-z") 'undo)
(define-key evil-insert-state-map (kbd "C-y") 'undo-redo)

