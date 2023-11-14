  (defvar materus/nix-packages nil)
  
  
  
  (when (not materus/nix-packages)
    (message "Not using config from nix packages, using straight")
    (defvar bootstrap-version)
    (let ((bootstrap-file
          (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
          (bootstrap-version 6))
      (unless (file-exists-p bootstrap-file)
        (with-current-buffer
            (url-retrieve-synchronously
            "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
            'silent 'inhibit-cookies)
          (goto-char (point-max))
          (eval-print-last-sexp)))
      (load bootstrap-file nil 'nomessage))
    (declare-function straight-use-package "straight" (&optional ARG))
    (straight-use-package 'load-relative)

    (straight-use-package 'use-package)
    (straight-use-package 'telephone-line)
    (straight-use-package 'elcord)
    (straight-use-package 'dashboard)
    (straight-use-package 'minions)
    (straight-use-package 'lsp-mode)
    (straight-use-package 'lsp-ui)
    
    (straight-use-package 'moe-theme)
    (straight-use-package 'doom-themes)
  )

