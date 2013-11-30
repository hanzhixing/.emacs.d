;;; Display
(tool-bar-mode -1)
(scroll-bar-mode -1)

;;; Encoding
(set-language-environment 'utf-8)
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;;; Line Feeding
(set-buffer-file-coding-system 'utf-8-unix)

;;; Font
(if (eq system-type 'gnu/linux)
    (set-face-attribute 'default nil :font "DejaVu Sans Mono" :height 120))
(if (eq system-type 'darwin)
    (set-face-attribute 'default nil :font "Monaco" :height 130))

;;; Tabs
(add-hook 'after-change-major-mode-hook 
	  '(lambda () 
             (setq-default indent-tabs-mode nil)
             (setq c-basic-indent 4)
             (setq tab-width 4)))

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
(add-to-list 'load-path "~/.emacs.d/site-lisp/tomorrow-theme/GNU Emacs")
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
(load "~/.emacs.d/site-lisp/nxhtml/autostart.el")

;;; Dired Plus
(add-to-list 'load-path "~/.emacs.d/site-lisp/dired-plus")
(require 'dired+)
