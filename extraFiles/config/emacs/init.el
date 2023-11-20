
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
(setq auto-save-list-file-prefix (concat user-emacs-directory "backups/"))
(setq backup-directory-alist
  `(("." . ,(concat user-emacs-directory "backups/"))))

(require 'telephone-line)
(require 'elcord)
(require 'dashboard)
(require 'minions)
(require 'doom-themes)


;Graphical
(when (display-graphic-p)
  (set-frame-font "FiraCode Nerd Font" nil t)
  
)



(setq read-process-output-max (* 1024 1024 3))
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
