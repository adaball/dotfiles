;; emacs config file

;;;;
;; initialize package installation logic
;;;;

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

;; ensure `use-package` is installed
(unless (package-installed-p 'use-package)
  (when (not package-archive-contents)
    (package-refresh-contents))
  (package-install 'use-package))

;;;;
;; utility functions
;;;;

(defun amb/replace-tabs-in-buffer ()
  "Replace tabs with spaces"
  (interactive)
  (set-variable 'tab-width 2)
  (mark-whole-buffer)
  (untabify (region-beginning) (region-end))
  (keyboard-quit))

(defun amb/fix-format-in-buffer ()
  "Re-indent entire buffer"
  (interactive)
  (save-excursion
    (mark-whole-buffer)
    (indent-region (region-beginning) (region-end))
    (keyboard-quit)))

(defun amb/toggle-comment-on-line ()
  "Comment or uncomment current line"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))

(defun amb/install-package-if-missing (package)
  "Perform package installation if package is missing"
  (unless (package-installed-p package)
    (when (not package-archive-contents)
      (package-refresh-contents))
    (package-install package)))

(defun amb/friendlier-visible-bell ()
  "A friendlier visual bell effect. (https://www.emacswiki.org/emacs/AlarmBell#toc8)"
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil 'invert-face 'mode-line))

(defun amb/delete-process-interactive ()
  "Delete process using an ido-read buffer (http://stackoverflow.com/questions/11572934/how-do-i-kill-a-running-process-in-emacs#11573495)"
  (interactive)
  (let ((pname (ido-completing-read "Process Name: " (mapcar 'process-name (process-list)))))
    (delete-process (get-process pname))))

(defun amb/set-font-height (h)
  "Interactively change font height attribute"
  (interactive (list (read-number "Enter new height: " (amb/cur-font-height))))
  (set-face-attribute 'default nil :height h))

(defun amb/cur-font-height ()
  "Return the current font height value"
  (let* ((d-attrs (face-all-attributes 'default))
         (h-pred  (lambda (v) (eq :height (car v))))
         (h-attr  (seq-filter h-pred d-attrs)))
    (cdr (car h-attr))))

(defun amb/is-windows-p ()
  "Confirm if the running OS is Windows"
  (string= system-type "windows-nt"))

(defun amb/is-macos-p ()
  "Confirm if the running OS is macOS"
  (string= system-type "darwin"))

(defun amb/url-un-escape-region (start end)
  "URL un-escape the current selected text"
  (interactive "r")
  (if (eq nil 'url-unhex-string)
      (require 'url-util))
  (save-excursion
    (let ((sub-str (buffer-substring-no-properties start end)))
      (delete-region start end)
      (goto-char start)
      (insert (url-unhex-string sub-str)))))

(defun amb/convert-from-epoch-region (start end)
  "Convert epoch timestamp in region to `Y-m-d H:M:S`"
  (interactive "r")
  (save-excursion
    (let ((sub-str (buffer-substring-no-properties start end)))
      (delete-region start end)
      (goto-char start)
      (insert (format-time-string "%Y-%m-%d %H:%M:%S" (string-to-number sub-str))))))

(defun amb/gen-scratch-buffer (&optional switch)
  "Make a new scratch buffer.  If called interactively, switch to the new buffer."
  (interactive (list 't))
  (let ((buffer (generate-new-buffer (make-temp-name "scratch-"))))
    (with-current-buffer buffer
      (org-mode)
      (undo-tree-mode))
    (if switch (switch-to-buffer buffer) buffer)))

;;;;
;; OS specific
;;;;

;; set some OS specific backups
(when (amb/is-windows-p) (set-fontset-font t nil "Consolas" nil 'append))
(when (amb/is-macos-p) (set-fontset-font t nil "Monaco" nil 'append))

;;;;
;; miscellaneous, pre-package load
;;;;

;; disable deprecation warnings about the `cl` library
(setq byte-compile-warnings '(cl-functions))

;;;;
;; install and configure packages
;;;;

(use-package base16-theme
  :config
  (load-theme 'base16-ashes t)
  :ensure t)

(use-package company
  :config
  (require 'company)
  (add-hook 'after-init-hook #'global-company-mode)
  :ensure t)

(use-package evil
  :bind (("C-h" . evil-window-left)
         ("C-l" . evil-window-right)
         ("C-j" . evil-window-down)
         ("C-k" . evil-window-up))
  :config
  (require 'evil)
  (evil-mode 1)
  (add-hook 'package-menu-mode-hook #'turn-on-evil-mode)
  (evil-set-undo-system 'undo-tree)
  (evil-set-leader 'normal ",")

  (evil-define-key 'normal 'global (kbd "<leader>w") 'save-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>q") 'kill-buffer)
  (evil-define-key 'normal 'global (kbd "C-S-h") 'help)
  :demand t
  :ensure t)

(use-package org
  :config
  (global-set-key (kbd "C-c c") #'org-capture)
  (global-set-key (kbd "C-c a") #'org-agenda)
  (setq org-directory "~/Documents/org")
  (setq gtd-file (concat (file-name-as-directory org-directory) "gtd.org"))
  (setq org-agenda-files (list gtd-file))
  (setq org-capture-templates '(("i" "Inbox" entry (file+headline gtd-file "Inbox")
                                 "* TODO %i%? \n %U"))))

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  :ensure t)

(use-package undo-tree
  :config
  (global-undo-tree-mode)
  :ensure t)

;;;;
;; emacs general / ui settings
;;;;

;; load-path additions on work laptop
(when (amb/is-macos-p) 
  (add-to-list 'load-path "~/bin")
  (add-to-list 'load-path "/usr/bin")
  (add-to-list 'load-path "/opt/homebrew/bin/"))

;; set up the initial scratch buffer
(setq initial-scratch-message nil)
(defun init-scratch ()
  (amb/gen-scratch-buffer nil))
(setq initial-buffer-choice #'init-scratch)

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
(setq uniquify-buffer-name-style 'forward)

;; mouse scrolling fix
(setq mouse-wheel-scroll-amount '(4 ((shift) . 4) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)

;; don't show the welcome message
(setq inhibit-startup-message t)

;; global highlight lines
(global-hl-line-mode 1)

;; use friendlier visual bell
(setq visible-bell nil)
(setq ring-bell-function #'amb/friendlier-visible-bell)

;; matching parens highlight
(show-paren-mode 1)

;; global line numbers
(setq linum-format " %03d")
(global-display-line-numbers-mode 1)

;; no cursor blinking
(blink-cursor-mode 0)

;; full path in title bar
(setq-default frame-title-format "%b (%f)")

;; set default font height
(let ((size (if (amb/is-windows-p) 120 140)))
  (amb/set-font-height size))

;; override custom file
(setq custom-file "~/.emacs.d/custom.el")
(if (not (file-exists-p custom-file))
    (write-region "" nil custom-file))
(load custom-file)

;; use aspell
(setq-default ispell-program-name "ispell")

;;;;
;; key bindings
;;;;

;; fix format of buffer
(global-set-key (kbd "C-c f") 'amb/fix-format-in-buffer)

;; toggle flyspell-mode
(global-set-key (kbd "C-c s") 'flyspell-mode)

;; view the yank menu
(global-set-key (kbd "C-c y")
                '(lambda ()
                   (interactive)
                   (popup-menu 'yank-menu)))

;; word count of the current buffer
(global-set-key (kbd "C-x w") 'count-words)

