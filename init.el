;;; init.el --- Emacs init file

;; Author: zhixing.han.0409@gmail.com

;;; Commentary:
;; Emacs init file.

;;; Code:



;; Check OS
(if (not (eq system-type 'darwin)) (error "Only for OS X!"))

;; Check version

(if (version<= emacs-version "24.4") (error "Only for 24.4 or higher version of Emacs!"))

;;; ELPA.
(require 'package)
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(defvar my-packages '(exec-path-from-shell
                      wgrep
                      scratch
                      mwe-log-commands
                      color-theme-approximate
                      paredit
                      idle-highlight-mode
                      ido
                      ido-ubiquitous
                      idomenu
                      smex
                      neotree
                      dired+
                      dired-sort
                      magit
                      find-file-in-project
                      scpaste
                      diff-hl
                      anzu
                      flycheck
                      evil
                      auto-complete
                      mmm-mode
                      unfill
                      ag
                      wgrep-ag
                      expand-region
                      multiple-cursors
                      move-dup
                      highlight-escape-sequences
                      guide-key
                      diff-hl
                      markdown-mode
                      csv-mode
                      csv-nav
                      json-mode
                      js2-mode
                      ac-js2
                      js-comint
                      php-mode
                      smarty-mode
                      ))

(package-initialize)

(dolist (pkg my-packages)
  (when (not (package-installed-p pkg))
    (if (not (assoc pkg package-archive-contents))
        (package-refresh-contents)
      )
    (package-install pkg)))

;;; $PATH

(require 'exec-path-from-shell)
(exec-path-from-shell-initialize)





;;; Load configs for specific features and modes

(require 'cl-lib)
(require 'wgrep)
(require 'scratch)
(require 'mwe-log-commands)





;;; Modes

;; Keyboard
(if (eq system-type 'darwin)
  ;(setq mac-option-ctrl 'control)
  ;(setq mac-option-modifier 'meta)
  ;(setq mac-command-modifier 'super)
  ;(setq mac-function-modifier 'none)
  (setq default-input-method "MacOSX")
  ;; Make mouse wheel / trackpad scrolling less jerky
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 5) ((control))))
  (dolist (multiple '("" "double-" "triple-"))
    (dolist (direction '("right" "left"))
      (global-set-key (kbd (concat "<" multiple "wheel-" direction ">")) 'ignore)))
  )





;; Theme

(load-theme 'wombat)
(set-cursor-color "yellow")
(require 'color-theme-approximate)
(color-theme-approximate-on)





;; NeoTree
(require 'neotree)





;;; GUI

(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)
(setq indicate-empty-lines t)
(tool-bar-mode -1)
(set-scroll-bar-mode nil)

(add-to-list 'default-frame-alist '(internal-border-width . 0))
(add-to-list 'initial-frame-alist '(internal-border-width . 0))

(add-hook 'after-make-frame-functions
          (lambda (frame)
            (with-selected-frame frame
                                 (unless window-system
                                   (set-frame-parameter nil 'menu-bar-lines 0)))))

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                 (abbreviate-file-name (buffer-file-name))
                 "%b"))))

(add-hook 'term-mode-hook
          (lambda ()
            (setq line-spacing 0)))






;;; Dired

(require 'dired+)
(require 'dired-sort)

(setq-default diredp-hide-details-initially-flag nil
              dired-dwim-target t)

;; Prefer g-prefixed coreutils version of standard utilities when available
(let ((gls (executable-find "gls")))
  (when gls (setq insert-directory-program gls)))

(with-eval-after-load 'dired
                      (require 'dired+)
                      (require 'dired-sort)
                      (when (fboundp 'global-dired-hide-details-mode)
                        (global-dired-hide-details-mode -1))
                      (setq dired-recursive-deletes 'top)
                      (define-key dired-mode-map [mouse-2] 'dired-find-file)
                      (add-hook 'dired-mode-hook
                                (lambda () (guide-key/add-local-guide-key-sequence "%"))))

(require 'diff-hl)
(with-eval-after-load 'dired
                      (add-hook 'dired-mode-hook 'diff-hl-dired-mode))





;;; Search and replace

(require 'anzu)
(global-anzu-mode t)

(global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp)
(global-set-key [remap query-replace] 'anzu-query-replace)

;; Activate occur easily inside isearch
(define-key isearch-mode-map (kbd "C-o") 'isearch-occur)

;; DEL during isearch should edit the search string, not jump back to the previous result
(define-key isearch-mode-map [remap isearch-delete-char] 'isearch-del-char)

;; Search At Point. See http://www.emacswiki.org/emacs/SearchAtPoint
(defun isearch-yank-symbol ()
  "*Put symbol at current point into search string."
  (interactive)
  (let ((sym (symbol-at-point)))
    (if sym
      (progn
        (setq isearch-regexp t
              isearch-string (concat "\\_<" (regexp-quote (symbol-name sym)) "\\_>")
              isearch-message (mapconcat 'isearch-text-char-description isearch-string "")
              isearch-yank-flag t))
      (ding)))
  (isearch-search-and-update))

(define-key isearch-mode-map "\C-\M-w" 'isearch-yank-symbol)

;; Zap To I Search. See http://www.emacswiki.org/emacs/ZapToISearch
(defun zap-to-isearch (rbeg rend)
  "Kill the region between the mark and the closest portion of
  the isearch match string. The behaviour is meant to be analogous
  to zap-to-char; let's call it zap-to-isearch. The deleted region
  does not include the isearch word. This is meant to be bound only
  in isearch mode.  The point of this function is that oftentimes
  you want to delete some portion of text, one end of which happens
  to be an active isearch word. The observation to make is that if
  you use isearch a lot to move the cursor around (as you should,
                                                      it is much more efficient than using the arrows), it happens a
  lot that you could just delete the active region between the mark
  and the point, not include the isearch word."
  (interactive "r")
  (when (not mark-active)
    (error "Mark is not active"))
  (let* ((isearch-bounds (list isearch-other-end (point)))
         (ismin (apply 'min isearch-bounds))
         (ismax (apply 'max isearch-bounds))
         )
    (if (< (mark) ismin)
      (kill-region (mark) ismin)
      (if (> (mark) ismax)
        (kill-region ismax (mark))
        (error "Internal error in isearch kill function.")))
    (isearch-exit)
    ))

(define-key isearch-mode-map [(meta z)] 'zap-to-isearch)

;; See http://www.emacswiki.org/emacs/ZapToISearch
(defun isearch-exit-other-end (rbeg rend)
  "Exit isearch, but at the other end of the search string.
  This is useful when followed by an immediate kill."
  (interactive "r")
  (isearch-exit)
  (goto-char isearch-other-end))

(define-key isearch-mode-map [(control return)] 'isearch-exit-other-end)





;;; Grep

(setq-default grep-highlight-matches t
              grep-scroll-output t)

(if (eq system-type 'darwin)
  (setq-default locate-command "mdfind"))

(when (executable-find "ag")
  (require 'ag)
  (require 'wgrep-ag)
  (setq-default ag-highlight-search t)
  (global-set-key (kbd "M-?") 'ag-project))





;;; uniquify

(require 'uniquify)

(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator " • ")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")





;;; Flycheck

(require 'flycheck)
(add-hook 'after-init-hook 'global-flycheck-mode)
;; Override default flycheck triggers
(setq flycheck-check-syntax-automatically '(save idle-change mode-enabled)
      flycheck-idle-change-delay 0.8)

(setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)





;;; Evil

(require 'evil)
(evil-mode t)
(setq evil-default-state 'emacs)

(define-key evil-normal-state-map (kbd ",f") 'projectile-find-file)
(define-key evil-normal-state-map (kbd ",,") 'evil-buffer)
(define-key evil-normal-state-map (kbd "q") nil)

(define-key evil-insert-state-map (kbd "C-e") nil)
(define-key evil-insert-state-map (kbd "C-d") nil)
(define-key evil-insert-state-map (kbd "C-k") nil)
(define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
(define-key evil-visual-state-map (kbd "C-c") 'evil-normal-state)

(define-key evil-motion-state-map (kbd "C-e") nil)
(define-key evil-visual-state-map (kbd "C-c") 'evil-exit-visual-state)





;;; InteractivelyDoThings - ido

(require 'ido)
(ido-mode t)
(ido-everywhere t)
(setq ido-enable-flex-matching t)
(setq ido-use-filename-at-point nil)
(setq ido-auto-merge-work-directories-length 0)
(setq ido-use-virtual-buffers t)

(require 'ido-ubiquitous)
(ido-ubiquitous-mode t)

(require 'smex)
(setq smex-save-file (expand-file-name "temp/smex-items" user-emacs-directory))
(global-set-key [remap execute-extended-command] 'smex)

(require 'idomenu)

;; Allow the same buffer to be open in different frames
(setq ido-default-buffer-method 'selected-window)

;; http://www.reddit.com/r/emacs/comments/21a4p9/use_recentf_and_ido_together/cgbprem
(add-hook 'ido-setup-hook (lambda () (define-key ido-completion-map [up] 'previous-history-element)))





;;; Hippie Expand

(global-set-key (kbd "M-/") 'hippie-expand)

(setq hippie-expand-try-functions-list
      '(try-complete-file-name-partially
         try-complete-file-name
         try-expand-dabbrev
         try-expand-dabbrev-all-buffers
         try-expand-dabbrev-from-kill))





;;; Auto Complete

(require 'auto-complete)
(require 'auto-complete-config)
(global-auto-complete-mode t)
(setq-default ac-expand-on-auto-complete nil)
(setq-default ac-auto-start nil)
(setq-default ac-dwim nil) ; To get pop-ups with docs even if a word is uniquely completed

;; Use Emacs' built-in TAB completion hooks to trigger AC (Emacs >= 23.2)
(setq tab-always-indent 'complete)  ;; use 't when auto-complete is disabled
(add-to-list 'completion-styles 'initials t)
;; Stop completion-at-point from popping up completion buffers so eagerly
(setq completion-cycle-threshold 5)

;; TODO: find solution for php, haskell and other modes where TAB always does something

(setq c-tab-always-indent nil
      c-insert-tab-function 'indent-for-tab-command)

;; hook AC into completion-at-point
(defun init/auto-complete-at-point ()
  (when (and (not (minibufferp))
             (fboundp 'auto-complete-mode)
             auto-complete-mode)
    #'auto-complete))

(defun init/never-indent ()
  (set (make-local-variable 'indent-line-function) (lambda () 'noindent)))

(defun set-auto-complete-as-completion-at-point-function ()
  (setq completion-at-point-functions
        (cons 'init/auto-complete-at-point
              (remove 'init/auto-complete-at-point completion-at-point-functions))))

(add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)


(set-default 'ac-sources
             '(ac-source-imenu
                ac-source-dictionary
                ac-source-words-in-buffer
                ac-source-words-in-same-mode-buffers
                ac-source-words-in-all-buffer))

(dolist (mode '(log-edit-mode org-mode text-mode haml-mode
                              git-commit-mode
                              sass-mode yaml-mode csv-mode espresso-mode
                              html-mode nxml-mode sh-mode smarty-mode
                              lisp-mode textile-mode markdown-mode tuareg-mode
                              js3-mode css-mode less-css-mode sql-mode
                              sql-interactive-mode
                              inferior-emacs-lisp-mode))
  (add-to-list 'ac-modes mode))


;; Exclude very large buffers from dabbrev
(defun init/dabbrev-friend-buffer (other-buffer)
  (< (buffer-size other-buffer) (* 1 1024 1024)))

(setq dabbrev-friend-buffer-function 'init/dabbrev-friend-buffer)




;;; Fonts

;(set-face-attribute 'default nil :font "Monaco" :height 130))
;(set-face-attribute 'default nil :font "DejaVu Sans Mono" :height 120))





;;; Multiple Major Modes --- mmm

(require 'mmm-mode)
(require 'mmm-auto)
(setq mmm-global-mode 'buffers-with-submode-classes)
(setq mmm-submode-decoration-level 2)






;;; Editing utils

(require 'unfill)

(electric-pair-mode)
(electric-indent-mode 1)

;; Some basic preferences
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

(require 'undo-tree)
(global-undo-tree-mode)

;; Expand region
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)



;; Rectangle selections, and overwrite text when the selection is active
(cua-selection-mode t)                  ; for rectangles, CUA is nice

(require 'multiple-cursors)
;; multiple-cursors
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-+") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(require 'move-dup)
(global-move-dup-mode)

(require 'highlight-escape-sequences)
(hes-mode)

(require 'guide-key)
(setq guide-key/guide-key-sequence '("C-x" "C-c" "C-x 4" "C-x 5" "C-c ;" "C-c ; f" "C-c ' f" "C-x n" "C-x C-r" "C-x r"))
(guide-key-mode 1)

(require 'diff-hl)
(add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)
(add-hook 'vc-dir-mode-hook 'turn-on-diff-hl-mode)





;;; Major modes according file formats or programming languages

(require 'markdown-mode)


(require 'csv-mode)
(require 'csv-nav)

(add-to-list 'auto-mode-alist '("\\.[Cc][Ss][Vv]\\'" . csv-mode))

(setq csv-separators '("," ";" "|" " "))

(require 'json-mode)
(require 'js2-mode)
(require 'ac-js2)
(require 'js-comint)

(require 'php-mode)
(require 'smarty-mode)

;; nxml
(add-to-list 'auto-mode-alist '("\\.xml\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.xsd\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.sch\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.rng\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.xslt\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.svg\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.rss\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.gpx\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.tcx\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.plist\\'" . nxml-mode))

(setq magic-mode-alist (cons '("<\\?xml " . nxml-mode) magic-mode-alist))
(fset 'xml-mode 'nxml-mode)
(add-hook 'nxml-mode-hook (lambda ()
                            (set (make-local-variable 'ido-use-filename-at-point) nil)))
(setq nxml-slash-auto-complete-flag t)


;; See: http://sinewalker.wordpress.com/2008/06/26/pretty-printing-xml-with-emacs-nxml-mode/
(defun init/pp-xml-region (beg end)
  "Pretty format XML markup in region. The function inserts
  linebreaks to separate tags that have nothing but whitespace
  between them.  It then indents the markup by using nxml's
  indentation rules."
  (interactive "r")
  (unless (use-region-p)
    (setq beg (point-min)
          end (point-max)))
  ;; Use markers because our changes will move END
  (setq beg (set-marker (make-marker) begin)
        end (set-marker (make-marker) end))
  (save-excursion
    (goto-char beg)
    (while (search-forward-regexp "\>[ \\t]*\<" end t)
           (backward-char) (insert "\n"))
    (nxml-mode)
    (indent-region begin end)))


(require 'server)
(unless (server-running-p) (server-start))

;;; Locales (setting them earlier in this file doesn't work in X)
(setq utf-translate-cjk-mode t)
(setq locale-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;;; init.el ends here
