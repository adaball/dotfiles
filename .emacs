(add-to-list 'load-path "/usr/local/bin")
(add-to-list 'exec-path "~/bin/")

;;;;
;; utility functions
;;;;

(defun amb/replace-tabs ()
  "replace tabs with spaces"
  (interactive)
  (set-variable 'tab-width 2)
  (mark-whole-buffer)
  (untabify (region-beginning) (region-end))
  (keyboard-quit))

(defun amb/fix-format ()
  "re-indent entire buffer"
  (interactive)
  (mark-whole-buffer)
  (indent-region (region-beginning) (region-end))
  (keyboard-quit))

(defun amb/toggle-comment-on-line ()
  "comment or uncomment current line"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))

(defun amb/install-package-if-missing (package)
  "perform package installation if package is missing"
  (unless (package-installed-p package)
    (package-refresh-contents)
    (package-install package)))

(defun amb/friendlier-visible-bell ()
  "A friendlier visual bell effect. (https://www.emacswiki.org/emacs/AlarmBell#toc8)"
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil 'invert-face 'mode-line))

(defun amb/delete-process-interactive ()
  "delete process using an ido-read buffer (http://stackoverflow.com/questions/11572934/how-do-i-kill-a-running-process-in-emacs#11573495)"
  (interactive)
  (let ((pname (ido-completing-read "Process Name: " (mapcar 'process-name (process-list)))))
    (delete-process (get-process pname))))

;;;;
;; initialize package installation logic
;;;;
(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(package-initialize)

;;;;
;; load packages / install if missing
;;;;

;; golden-ratio
(amb/install-package-if-missing 'golden-ratio)
(require 'golden-ratio)

;; paredit
(amb/install-package-if-missing 'paredit)
(require 'paredit)

;; magit
(amb/install-package-if-missing 'magit)
(require 'magit)

;; rainbows!
(amb/install-package-if-missing 'rainbow-delimiters)
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; markdown mode
(amb/install-package-if-missing 'markdown-mode)
(require 'markdown-mode)

;; vimrc mode
(amb/install-package-if-missing 'vimrc-mode)
(require 'vimrc-mode)

;; web mode
(amb/install-package-if-missing 'web-mode)
(require 'web-mode)
(setq web-mode-code-indent-offset 2)

;; regex builder
(require 're-builder)
(setq reb-re-syntax 'string)

;; company mode
(amb/install-package-if-missing 'company)
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

;; evil mode
(amb/install-package-if-missing 'evil)
(evil-mode 1)

;; base16 themes
(amb/install-package-if-missing 'base16-theme)
(load-theme 'base16-ashes t)

;;;;
;; Clojure specific
;;;;

;; clojure mode
(amb/install-package-if-missing 'clojure-mode)
(amb/install-package-if-missing 'clojure-mode-extra-font-locking)

;; clojure mode hooks
(add-hook 'clojure-mode-hook #'paredit-mode)
(add-hook 'clojure-mode-hook #'eldoc-mode)

;; midje-mode
(amb/install-package-if-missing 'midje-mode)

;; cider
(amb/install-package-if-missing 'cider)

;; cider hooks
(add-hook 'cider-repl-mode-hook #'paredit-mode)
(add-hook 'cider-repl-mode-hook #'eldoc-mode)
(add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode)
(add-hook 'cider-repl-mode-hook #'midje-mode)

;; cider settings
(setq cider-prompt-save-file-on-load nil)
(setq nrepl-hide-special-buffers t)
(setq cider-repl-result-prefix ";; => ")

;; clear Cider's repl buffer with Ctrl+1
(eval-after-load 'cider-repl
  '(define-key cider-repl-mode-map
     (kbd "C-1") 'cider-repl-clear-buffer))

;;;;
;; Emacs Lisp specific
;;;;

;; paredit / eldoc in ielm
(add-hook 'ielm-mode-hook #'eldoc-mode)
(if (fboundp 'paredit-mode)
    (add-hook 'ielm-mode-hook #'paredit-mode))

;; paredit / eldoc in emacs lisp
(add-hook 'emacs-lisp-mode-hook #'eldoc-mode)
(if (fboundp 'paredit-mode)
    (add-hook 'emacs-lisp-mode-hook #'paredit-mode))

;;;;
;; JavaScript specific
;;;;

;; json mode
(amb/install-package-if-missing 'json-mode)
(require 'json-mode)
(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))

;; js ide mode
(amb/install-package-if-missing 'js2-mode)
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(setq js2-bounce-indent-p t)
(setq js2-basic-offset 2)

;;;;
;; Emacs environment settings
;;;;

;; don't use backup files
(setq make-backup-files nil)

;; set the initial mode and nix the scratch message
(setq initial-major-mode 'fundamental-mode)
(setq initial-scratch-message nil)

;; use y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; don't use hard tabs
(setq-default indent-tabs-mode nil)

;; no electric-indent
(setq electric-indent-mode nil)

;; get rid of toolbar, scrollbar, and menubar
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; column numbering
(column-number-mode t)

;; better handling of duplicate named files
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; mouse scrolling fix
(setq mouse-wheel-scroll-amount '(4 ((shift) . 4) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)

;; don't show the welcome message
(setq inhibit-startup-message t)

;; global highlight lines
(custom-set-faces
 '(hl-line ((t (:background "#393f45")))))
(global-hl-line-mode 1)

;; use friendlier visual bell
(setq visible-bell nil)
(setq ring-bell-function #'amb/friendlier-visible-bell)

;; matching parens highlight
(show-paren-mode 1)

;; global line numbers
(global-linum-mode 1)

;; no cursor blinking
(blink-cursor-mode 0)

;; full path in title bar
(setq-default frame-title-format "%b (%f)")

;; set font-face to 16
(set-face-attribute 'default nil :height 160)

;; override custom file
(setq custom-file "~/.emacs.d/custom.el")
(if (not (file-exists-p custom-file))
    (write-region "" nil custom-file))
(load custom-file)

;;;;
;; Global key bindings
;;;;

;; Link win (super) key to meta
(setq x-super-keysym 'meta)

;; fix format of buffer
(global-set-key (kbd "C-c C-f") 'amb/fix-format)

;; comment line with Ctrl-Shift-/
(global-set-key (kbd "C-?") 'amb/toggle-comment-on-line)

;; magit status for the current buffer
(global-set-key (kbd "C-x g") 'magit-status)
