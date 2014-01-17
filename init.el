;;; init.el --- init
;;; Display
;;; Code:

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
;; Insert spaces instead of tabs. Changes by modes. (TAB = What?)
(setq-default indent-tabs-mode nil)

;;; Theme
(load-theme 'wombat)
(set-cursor-color "yellow")

;;; ELPA. Added, but I prefer download packages manually. Because by this way, it seems more clean.
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))

(provide 'init)

;;; init.el ends here
