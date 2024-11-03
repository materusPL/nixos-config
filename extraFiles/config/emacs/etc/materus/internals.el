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

  




;;. Graphical
(setq frame-resize-pixelwise t)
(setq window-resize-pixelwise t)

(when (display-graphic-p)
  (set-frame-font "Hack Nerd Font" nil t)
  )


(xterm-mouse-mode 1)

(setq read-process-output-max (* 1024 1024 3))


(setq ring-bell-function 'ignore)
