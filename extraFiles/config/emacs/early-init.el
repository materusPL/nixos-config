


(defvar materus/init-early t)
(setq materus/init-early t)

(when (boundp 'native-comp-eln-load-path)
  (startup-redirect-eln-cache (expand-file-name "/var/eln-cache/" user-emacs-directory)))

(tool-bar-mode -1)
(setq initial-major-mode 'fundamental-mode)
(setq-default package-quickstart t)
(setq native-comp-speed 3)
(add-hook 'emacs-startup-hook (lambda () (package-initialize)
   (setq gc-cons-threshold 100000000 ; ~100mb
          gc-cons-percentage 0.1)
))
(setq package-enable-at-startup nil)
(unless (daemonp)
  (setq gc-cons-threshold most-positive-fixnum ; 2^61 bytes
      gc-cons-percentage 0.6)
  
  
  (advice-add #'tty-run-terminal-initialization :override #'ignore)
  (add-hook 'window-setup-hook
    (lambda ()
      (advice-remove #'tty-run-terminal-initialization #'ignore)
      (tty-run-terminal-initialization (selected-frame) nil t)
    )
  )
)

(when (and (fboundp 'startup-redirect-eln-cache)
           (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (startup-redirect-eln-cache
   (convert-standard-filename
    (expand-file-name  "var/eln-cache/" user-emacs-directory))))

(setq auto-save-default nil)          
(setq backup-directory-alist
      `((".*" . ,(concat user-emacs-directory "var/backups/"))))
(setq auto-save-file-name-transforms
      `((".*" ,(concat user-emacs-directory "var/recovery/") t)))