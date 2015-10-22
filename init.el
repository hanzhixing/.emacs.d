;;; init.el --- Emacs init file

;; Author: zhixing.han.0409@gmail.com

;;; Commentary:
;; Emacs init file.

;;; Code:



;;; Check OS
(if (not (eq system-type 'darwin)) (error "Only for OS X!"))

;;; Check version

(if (version<= emacs-version "24.4") (error "Only for 24.4 or higher version of Emacs!"))



;;; ELPA.
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(defvar my-packages '(
		      smex
		      exec-path-from-shell
		      which-key
		      evil
		      neotree
		      anzu
		      auto-complete
		      expand-region
		      multiple-cursors
		      move-dup
		      ))

(package-initialize)

(dolist (pkg my-packages)
  (when (not (package-installed-p pkg))
    (if (not (assoc pkg package-archive-contents))
	(package-refresh-contents)
      )
    (package-install pkg)))



;;; built-in
(require 'cl-lib)
(require 'ido)
(ido-mode t)
(require 'server)
(unless (server-running-p) (server-start))



;;; built-in-extending
(when (package-installed-p 'smex)
  (require 'smex)
  (smex-initialize)
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
  ;; This is your old M-x.
  (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
  )



;;; exec-path-from-shell
(when (package-installed-p 'exec-path-from-shell)
  (require 'exec-path-from-shell)
  (exec-path-from-shell-initialize)
  )



;;; which-key
(when (package-installed-p 'which-key)
  (require 'which-key)
  (which-key-mode)
  (which-key-setup-side-window-right-bottom)
  (which-key-setup-minibuffer)
  )



;;; evil
(when (package-installed-p 'evil)
  (require 'evil)
  (evil-mode t)
  (setq evil-default-state 'emacs)
  (global-undo-tree-mode)
  )



;;; neotree
(when (package-installed-p 'evil)
  (require 'neotree)
  )


;;; anzu
(when (package-installed-p 'anzu)
  (require 'anzu)
  (global-anzu-mode t)
  (global-set-key (kbd "M-%") 'anzu-query-replace)
  (global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp)
  )



;;; auto-complete
(when (package-installed-p 'auto-complete)
  (ac-config-default)
  )



;;; expand-region
(when (package-installed-p 'expand-region)
  (require 'expand-region)
  (global-set-key (kbd "C-=") 'er/expand-region)
  )



;;; multiple-cursors
(when (package-installed-p 'multiple-cursors)
  (require 'multiple-cursors)
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
  )



;;; move-dup
(when (package-installed-p 'move-dup)
  (require 'move-dup)
  (global-set-key (kbd "M-<up>") 'md/move-lines-up)
  (global-set-key (kbd "M-<down>") 'md/move-lines-down)
  (global-set-key (kbd "C-M-<up>") 'md/duplicate-up)
  (global-set-key (kbd "C-M-<down>") 'md/duplicate-down)
  )




;;; theme
(load-theme 'wombat)
(set-cursor-color "green")
(add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-10"))
(tool-bar-mode -1)
(set-scroll-bar-mode nil)
(setq scroll-step 1)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-follow-mouse 't)
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)
(setq indicate-empty-lines t)
(electric-pair-mode)
(electric-indent-mode 1)
(setq blink-cursor-interval 0.4)
(setq bookmark-default-file (expand-file-name ".bookmarks.el" user-emacs-directory))
(setq buffers-menu-max-size 30)
(setq case-fold-search t)
(setq column-number-mode t)
(setq delete-selection-mode t)
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq indent-tabs-mode nil)
(setq make-backup-files nil)
(setq mouse-yank-at-point t)
(setq save-interprogram-paste-before-kill t)
(setq scroll-preserve-screen-position 'always)
(setq set-mark-command-repeat-pop t)
(setq show-trailing-whitespace t)
(setq tooltip-delay 1.5)
(setq truncate-lines nil)
(setq truncate-partial-width-windows nil)
(setq visible-bell t)
(setq show-paren-mode 1)
(global-auto-revert-mode)
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)
(transient-mark-mode t)
(global-prettify-symbols-mode)
(cua-selection-mode t)



;;; Locales (setting them earlier in this file doesn't work in X)
;(prefer-coding-system 'utf-8)
;(set-language-environment 'UTF-8)
;(set-default-coding-systems 'utf-8)

;;; init.el ends here
