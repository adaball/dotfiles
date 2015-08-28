;; proxy
(setq url-proxy-services
      '(("no_proxy" . "^\\(localhost\\|10.*\\)")
        (getenv "http_proxy")))

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

;; set theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/vendor")
(if (file-exists-p "~/.emacs.d/vendor/zerodark-theme.el")
    (load-theme 'zerodark t)
  (load-theme 'wombat))


;; matching parens highlight
(show-paren-mode 1)

;; global line numbers
(global-linum-mode 1)

;; no cursor blinking
(blink-cursor-mode 0)

;; full path in title bar
(setq-default frame-title-format "%b (%f)")

;; set font-face to 12
(set-face-attribute 'default nil :height 140)

;; get rid of toolbar, scrollbar, and menubar
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

;;;;
;; Editing
;;;;

;; don't use hard tabs
(setq-default indent-tabs-mode nil)

;; no electric-indent
(setq electric-indent-mode nil)

;; comments
(defun toggle-comment-on-line ()
  "comment or uncomment current line"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
(global-set-key (kbd "C-?") 'toggle-comment-on-line)

;;;;
;; Hooks
;;;;

;; company mode
(if (fboundp 'company-mode)
    (add-hook 'after-init-hook 'global-company-mode))

;; paredit / eldoc in cider
(if (fboundp 'cider-mode)
    (progn
      (add-hook 'cider-repl-mode-hook #'paredit-mode)
      (add-hook 'cider-repl-mode-hook #'eldoc-mode)))

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
(if (fboundp 'paredit-mode)
    (add-hook 'emacs-lisp-mode-hook #'paredit-mode))
(add-hook 'emacs-lisp-mode-hook #'eldoc-mode)

;; rainbows!
(if (fboundp 'rainbow-delimiters-mode)
    (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;;;;
;; Misc
;;;;

;; don't use backup files
(setq make-backup-files nil)

;; don't show the welcome message
(setq inhibit-startup-message t)

;; use y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; replace tabs with spaces
(defun replace-tabs ()
  (interactive)
  (set-variable 'tab-width 2)
  (mark-whole-buffer)
  (untabify (region-beginning) (region-end))
  (keyboard-quit))

;; re-indent whole buffer
(defun fix-format ()
  (interactive)
  (mark-whole-buffer)
  (indent-region (region-beginning) (region-end))
  (keyboard-quit))


;;;;
;; Cider / nRepl
;;;;

(setq cider-prompt-save-file-on-load nil)
(setq nrepl-hide-special-buffers t)
(setq cider-repl-result-prefix ";; => ")

