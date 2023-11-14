


(defvar materus/init-early t)
(setq materus/init-early t)

(tool-bar-mode -1)
(setq initial-major-mode 'fundamental-mode)
(setq-default package-quickstart t)
(setq native-comp-speed 3)

(unless (daemonp)
  (setq gc-cons-threshold most-positive-fixnum ; 2^61 bytes
      gc-cons-percentage 0.6)
  (setq  package-enable-at-startup nil)
  
  (add-hook 'emacs-startup-hook (lambda () (package-initialize)
   (setq gc-cons-threshold 16777216 ; 16mb
          gc-cons-percentage 0.1)
   ))

  
  (advice-add #'tty-run-terminal-initialization :override #'ignore)
  (add-hook 'window-setup-hook
    (lambda ()
      (advice-remove #'tty-run-terminal-initialization #'ignore)
      (tty-run-terminal-initialization (selected-frame) nil t)
    )
  )
)
