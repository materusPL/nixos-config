(defvar materus/init-early t) 			; Var to ensure early-init loaded
(setq materus/init-early t)			; Probably useless

(setq initial-major-mode 'fundamental-mode)
(setq native-comp-async-report-warnings-errors nil)
(setq package-enable-at-startup nil)

(setq native-comp-speed 3)
(add-hook 'emacs-startup-hook (lambda () (package-initialize)
   (setq gc-cons-threshold 100000000 ; ~100mb
          gc-cons-percentage 0.1)
))
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

(when (boundp 'native-comp-eln-load-path)                        ; Change dir for eln-cache
  (startup-redirect-eln-cache (expand-file-name "/var/eln-cache/" user-emacs-directory))) 

(when (and (fboundp 'startup-redirect-eln-cache)
           (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (startup-redirect-eln-cache
   (convert-standard-filename
    (expand-file-name  "var/eln-cache/" user-emacs-directory))))

(setq auto-save-default nil)          
(setq backup-directory-alist
      `((".*" . ,(concat user-emacs-directory "var/backups/"))))  ; Change backup and auto save dir to var dir 
(setq auto-save-file-name-transforms                              	
      `((".*" ,(concat user-emacs-directory "var/recovery/") t))) 
(setq auto-save-list-file-prefix (concat user-emacs-directory "var/auto-save/sessions/"))
(setq custom-file (concat user-emacs-directory "etc/custom.el"))
