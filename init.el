;;; ELPA. Added, but I prefer download packages manually. Because by this way, it seems more clean.
(require 'package)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))

;;; site-list directory.
;; ~/.emacs.d/site-lisp

;;; Evil.
(add-to-list 'load-path "~/.emacs.d/site-lisp/evil")
(require 'evil)
(evil-mode 1)

;;; Color Theme
(add-to-list 'load-path "~/.emacs.d/site-lisp/tomorrow-theme-master/GNU Emacs")
(require 'tomorrow-night-eighties-theme)
