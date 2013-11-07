;;; Display
(tool-bar-mode -1)
(scroll-bar-mode -1)

;;; Font
(set-face-attribute 'default nil :font "DejaVu Sans Mono-12")

;;; Default load-path
(add-to-list 'load-path "~/.emacs.d/site-lisp")

;;; ELPA. Added, but I prefer download packages manually. Because by this way, it seems more clean.
;(require 'package)
;(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
;(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
;(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))

;;; Evil.
(add-to-list 'load-path "~/.emacs.d/site-lisp/evil")
(require 'evil)
(evil-mode 1)

;;; Color Theme
;(add-to-list 'load-path "~/.emacs.d/site-lisp/color-theme")
;(require 'color-theme)
;(color-theme-initialize)
;(color-theme-zenburn)
(add-to-list 'load-path "~/.emacs.d/site-lisp/tomorrow-theme-master/GNU Emacs")
(require 'tomorrow-night-eighties-theme)

;;; nXhtml
;; Workaround the annoying warnings:
;; Warning (mumamo-per-buffer-local-vars):
;; Already 'permanent-local t: buffer-file-name
(when (and (equal emacs-major-version 24)
	   (equal emacs-minor-version 2))
  (eval-after-load "mumamo"
    '(setq mumamo-per-buffer-local-vars
	   (delq 'buffer-file-name mumamo-per-buffer-local-vars))))
(load "~/.emacs.d/site-lisp/nxhtml/autostart")

;;; Dired Plus
(require 'dired+)
