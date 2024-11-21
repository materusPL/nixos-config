;;; corfu-mouse.el --- Mouse support for Corfu -*- lexical-binding: t; -*-

;; Copyright (C) 2021, 2022  Free Software Foundation, Inc.
;; Copyright (C) 2022  Akib Azmain Turja.

;; Author: Akib Azmain Turja <akib@disroot.org>
;; Maintainer: Akib Azmain Turja <akib@disroot.org>
;; Created: 2022
;; Version: 0.1
;; Package-Requires: ((emacs "24.4") (corfu "0.25"))
;; Homepage: https://codeberg.org/akib/emacs-corfu-mouse

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; Disclaimer: This file is based on vertico-mouse.el of Vertico
;; package, which is a part of GNU Emacs.

;;; Commentary:

;; This package is a Corfu extension, which adds mouse support.

;; To enable, M-x corfu-mouse-mode.


;; Modified to support pixel-scroll-precision-mode and fixed adding spaces 

;;; Code:

(require 'corfu)
(defgroup corfu-mouse nil
  "Mouse support for Corfu."
  :group 'corfu
  :link '(url-link "https://codeberg.org/akib/emacs-corfu-mouse")
  :prefix "corfu-mouse-")

(defface corfu-mouse
  '((t :inherit highlight))
  "Face used for mouse highlighting."
  :group 'corfu-mouse)

(defvar corfu-mouse--completion-buffer nil
  "The buffer for which the popup is being shown.")

(defun corfu-mouse--candidate-map (index)
  "Return keymap for candidate with INDEX."
  (let ((map (make-sparse-keymap))
        (mouse-1 (make-symbol "corfu-mouse--on-mouse-1"))
        (mouse-3 (make-symbol "corfu-mouse--on-mouse-3")))
    (fset mouse-1 (lambda ()
                    (interactive)
                    (corfu--goto index)
                    (corfu-insert)))
    (fset mouse-3 (lambda ()
                    (interactive)
                    (corfu--goto index)
                    (corfu-complete)))
    (define-key map [mouse-1] mouse-1)
    (define-key map [mouse-3] mouse-3)

    ;; Ignore these events to keep completion session alive.
    (define-key map [down-mouse-1] #'ignore)
    (define-key map [down-mouse-3] #'ignore)
    map))

(defun corfu-mouse--format-candidates (fcands)
  "Format candidates.
   FCANDS is the return value of `corfu--format-candidates'."
  (let ((index corfu--scroll)
        (cands (caddr fcands)))
    (while cands
      (let ((line (car cands)))
        ;; Append necessary amount of spaces to make it as wide as the
        ;; popup.
        (let ((strlen (- (cadr fcands) (string-width line))))
          (when (> strlen 0)
            (setq line (concat line (make-string strlen ? )))))
        
        (add-text-properties 0 (length line)
                             `(mouse-face
                               corfu-mouse
                               keymap
                               ,(corfu-mouse--candidate-map index))
                             line)
        (setcar cands line)
        (setq cands (cdr cands))
        (setq index (1+ index))))
    fcands))

(defun corfu-mouse--scroll-up (n)
  "Scroll up by N lines."
  (with-current-buffer corfu-mouse--completion-buffer
    (corfu-next n)))

(defun corfu-mouse--scroll-down (n)
  "Scroll down by N lines."
  (corfu-mouse--scroll-up (- n)))

(defun corfu-mouse-mwheel-scroll ()
  "Call `mwheel-scroll'."
  (interactive)
  (call-interactively #'mwheel-scroll))

(defun corfu-mouse--setup-scrolling (buffer)
  "Setup mouse scrolling on BUFFER."
  (let ((current-buffer (current-buffer)))
    (with-current-buffer buffer
      (when (boundp 'pixel-scroll-precision-mode)
        (setq-local pixel-scroll-precision-mode nil))
      (setq-local mwheel-scroll-up-function #'corfu-mouse--scroll-up)
      (setq-local mwheel-scroll-down-function
                  #'corfu-mouse--scroll-down)
      (setq-local corfu-mouse--completion-buffer current-buffer)))
  buffer)

(defun corfu-mouse--post-command-set-buffer ()
  "Set `corfu-mouse--completion-buffer' the current buffer."
  (when corfu-mouse--completion-buffer
    (switch-to-buffer corfu-mouse--completion-buffer)))

;;;###autoload
(define-minor-mode corfu-mouse-mode
  "Mouse support for Corfu."
  :global t :group 'corfu
  (let ((scroll-events '(wheel-up wheel-down mouse-4 mouse-5))
        (continue-commands '("corfu-mouse--on-mouse-1"
                             "corfu-mouse--on-mouse-3"
                             corfu-mouse-mwheel-scroll)))
    (cond
     (corfu-mouse-mode
      (advice-add #'corfu--format-candidates :filter-return
                  #'corfu-mouse--format-candidates)
      (advice-add #'corfu--make-buffer :filter-return
                  #'corfu-mouse--setup-scrolling)
      (advice-add #'corfu--post-command :before
                  #'corfu-mouse--post-command-set-buffer)
      (setq corfu-continue-commands
            (append corfu-continue-commands continue-commands))
      (dolist (event scroll-events)
        (define-key corfu--mouse-ignore-map (vector event)
                    #'corfu-mouse-mwheel-scroll)))
     (t
      (advice-remove #'corfu--format-candidates
                     #'corfu-mouse--format-candidates)
      (advice-remove #'corfu--make-buffer
                     #'corfu-mouse--setup-scrolling)
      (advice-remove #'corfu--post-command
                     #'corfu-mouse--post-command-set-buffer)
      (dolist (command continue-commands)
        (setq corfu-continue-commands
              (delete command corfu-continue-commands)))
      (dolist (event scroll-events)
        (define-key corfu--mouse-ignore-map (vector event)
                    #'ignore))))))

(provide 'corfu-mouse)
;;; corfu-mouse.el ends here
