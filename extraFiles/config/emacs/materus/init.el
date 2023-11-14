(defvar materus/nix-packages nil)
(defvar materus/init-from-home nil)
(defvar materus/home-dir (concat user-emacs-directory "materus/" ))
(setq native-comp-async-report-warnings-errors nil)
(setq package-enable-at-startup nil)
(setq frame-inhibit-implied-resize t)


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
  (declare-function load-relative "load-relative" (&optional ARG))
  (straight-use-package 'load-relative)
  (load-relative "packages")
)

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
