;; override custom file
(setq custom-file "~/.emacs.d/custom.el")
(if (not (file-exists-p custom-file))
    (write-region "" nil custom-file))
(load custom-file)

;;;;
;; Packages
;;;;

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;; just for my reference, no auto-install
(defvar my-packages
  '(paredit
    clojure-mode-extra-font-locking
    cider
    rainbow-delimiters))

;; load-path
(add-to-list 'load-path "~/.emacs.d/vendor")

;;;;
;; UI
;;;;

;; mouse scrolling fix, smoother, by 2 lines at a time
(setq mouse-wheel-scroll-amount '(2 ((shift) . 2) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)

;; don't show the welcome message
(setq inhibit-startup-message t)

;; use visual indication instead of system bell
(setq visible-bell 1)

;; load wombat theme
(load-theme 'wombat)

;; matching parens highlight
(show-paren-mode 1)

;; global line numbers
(global-linum-mode 1)

;; no cursor blinking
(blink-cursor-mode 0)

;; full path in title bar
(setq-default frame-title-format "%b (%f)")

;; set font-face to 10
(set-face-attribute 'default nil :height 100)

;; get rid of toolbar, scrollbar, and menubar
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;;;;
;; Editing
;;;;

;; don't use hard tabs
(setq-default indent-tabs-mode nil)

;; no electric-indent
(setq electric-indent-mode nil)

;;;;
;; Mode Hooks
;;;;

;; company mode
(if (fboundp 'company-mode)
    (add-hook 'after-init-hook 'global-company-mode))

;; paredit / eldoc in cider
(if (fboundp 'cider-mode)
    (progn
      (add-hook 'cider-mode-hook '(lambda () (cider-loading-things)))
      (add-hook 'cider-repl-mode-hook #'paredit-mode)
      (add-hook 'cider-repl-mode-hook #'eldoc-mode)
      (add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode)))

;; paredit / eldoc in clojure-mode
(if (fboundp 'clojure-mode)
    (progn
      (add-hook 'clojure-mode-hook #'paredit-mode)
      (add-hook 'clojure-mode-hook #'eldoc-mode)))

;; paredit / eldoc in ielm
(add-hook 'ielm-mode-hook #'eldoc-mode)
(if (fboundp 'paredit-mode)
    (add-hook 'ielm-mode-hook #'paredit-mode))

;; paredit / eldoc in emacs lisp
(add-hook 'emacs-lisp-mode-hook #'eldoc-mode)
(if (fboundp 'paredit-mode)
    (add-hook 'emacs-lisp-mode-hook #'paredit-mode))

;; rainbows!
(if (fboundp 'rainbow-delimiters-mode)
    (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;;;;
;; Cider / nRepl specific
;;;;

(if (fboundp 'cider-mode)
    (progn
      (setq cider-prompt-save-file-on-load nil)
      (setq nrepl-hide-special-buffers t)
      (setq cider-repl-result-prefix ";; => ")
      (global-set-key (kbd "C-1") 'cider-repl-clear-buffer)))

;;;;
;; Misc
;;;;

;; link win (super) key to meta
(setq x-super-keysym 'meta)

;; don't use backup files
(setq make-backup-files nil)

;; use y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;;;;
;; Custom Functions
;;;;

(defun replace-tabs ()
  "replace tabs with spaces"
  (interactive)
  (set-variable 'tab-width 2)
  (mark-whole-buffer)
  (untabify (region-beginning) (region-end))
  (keyboard-quit))

(defun fix-format ()
  "re-indent entire buffer"
  (interactive)
  (mark-whole-buffer)
  (indent-region (region-beginning) (region-end))
  (keyboard-quit))

(defun toggle-comment-on-line ()
  "comment or uncomment current line"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))

;; comment line with Ctrl-Shift-/
(global-set-key (kbd "C-?") 'toggle-comment-on-line)
