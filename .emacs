;; emacs config file

;;;;
;; initialize package installation logic
;;;;

(require 'package)

(add-to-list 'package-archives 
             '("melpa" . "https://melpa.org/packages/") t)

(when (< emacs-major-version 27)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
  (package-initialize))

;;;;
;; utility functions
;;;;

(defun util/replace-tabs-in-buffer ()
  "Replace tabs with spaces"
  (interactive)
  (set-variable 'tab-width 2)
  (mark-whole-buffer)
  (untabify (region-beginning) (region-end))
  (keyboard-quit))

(defun util/fix-format-in-buffer ()
  "Re-indent entire buffer"
  (interactive)
  (mark-whole-buffer)
  (indent-region (region-beginning) (region-end))
  (keyboard-quit))

(defun util/toggle-comment-on-line ()
  "Comment or uncomment current line"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))

(defun util/install-package-if-missing (package)
  "Perform package installation if package is missing"
  (unless (package-installed-p package)
    (when (not package-archive-contents)
      (package-refresh-contents))
    (package-install package)))

(defun util/friendlier-visible-bell ()
  "A friendlier visual bell effect. (https://www.emacswiki.org/emacs/AlarmBell#toc8)"
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil 'invert-face 'mode-line))

(defun util/delete-process-interactive ()
  "Delete process using an ido-read buffer (http://stackoverflow.com/questions/11572934/how-do-i-kill-a-running-process-in-emacs#11573495)"
  (interactive)
  (let ((pname (ido-completing-read "Process Name: " (mapcar 'process-name (process-list)))))
    (delete-process (get-process pname))))

(defun util/set-font-height ()
  "Interactively change font height attribute"
  (interactive
   (let* ((current-height (face-attribute 'default :height))
          (updated-height (read-from-minibuffer
                           (format "Enter new height (current is %d): " current-height))))
     (set-face-attribute 'default nil :height (string-to-number updated-height)))))

(defun util/is-windows-p ()
  "Confirm if the running OS is Windows"
  (string= system-type "windows-nt"))

(defun util/is-macos-p ()
  "Confirm if the running OS is macOS"
  (string= system-type "darwin"))

(defun util/url-escape-region (start end)
  "URL escape the current selected text"
  (interactive "r")
  (if (eq nil 'url-unhex-string)
      (require 'url-util))
  (save-excursion
    (let ((sub-str (buffer-substring-no-properties start end)))
      (delete-region start end)
      (goto-char start)
      (insert (url-unhex-string sub-str)))))

;;;;
;; OS specific
;;;;

;; set font face to Monaco on work laptop
(if (and (util/is-macos-p) (member "Monaco" (font-family-list)))
    (set-face-attribute 'default nil :font "Monaco"))

;; set font face to Source Code Pro if we're on Windows and it's installed
(if (and (util/is-windows-p) (member "Source Code Pro" (font-family-list)))
    (set-face-attribute 'default nil :font "Source Code Pro"))

;; set Emacs C source directory on home desktop
(if (and (util/is-windows-p) (file-exists-p "c:/Users/adam/bin/emacs-27.2-src/"))
    (setq source-directory "c:/Users/adam/bin/emacs-27.2-src/"))

;;;;
;; miscellaneous, pre-package load
;;;;

;; disable deprecation warnings about the `cl` library
(setq byte-compile-warnings '(cl-functions))

;;;;
;; install packages
;;;;

(util/install-package-if-missing 'base16-theme)
(util/install-package-if-missing 'cider)
(util/install-package-if-missing 'clhs)
(util/install-package-if-missing 'clojure-mode)
(util/install-package-if-missing 'clojure-mode-extra-font-locking)
(util/install-package-if-missing 'company)
(util/install-package-if-missing 'evil)
(util/install-package-if-missing 'json-mode)
(util/install-package-if-missing 'magit)
(util/install-package-if-missing 'markdown-mode)
(util/install-package-if-missing 'midje-mode)
(util/install-package-if-missing 'paredit)
(util/install-package-if-missing 'rainbow-delimiters)
(util/install-package-if-missing 'slime)
(util/install-package-if-missing 'sql-indent)
(util/install-package-if-missing 'undo-tree)
(util/install-package-if-missing 'vlf)
(util/install-package-if-missing 'wiki-summary)

;;;;
;; requires / hooks / init
;;;;

(require 'company)
(require 'evil)
(require 'json-mode)
(require 'magit)
(require 'markdown-mode)
(require 'paredit)
(require 'rainbow-delimiters)
(require 'undo-tree)
(require 'uniquify)

(evil-mode 1)

(add-hook 'after-init-hook 'global-company-mode)
(add-hook 'cider-repl-mode-hook #'eldoc-mode)
(add-hook 'cider-repl-mode-hook #'midje-mode)
(add-hook 'cider-repl-mode-hook #'paredit-mode)
(add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook #'eldoc-mode)
(add-hook 'clojure-mode-hook #'paredit-mode)
(add-hook 'emacs-lisp-mode-hook #'eldoc-mode)
(add-hook 'emacs-lisp-mode-hook #'paredit-mode)
(add-hook 'ielm-mode-hook #'eldoc-mode)
(add-hook 'ielm-mode-hook #'paredit-mode)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; normal <delete> key binding in paredit
(eval-after-load 'paredit
  '(progn
     (define-key paredit-mode-map (kbd "<delete>") nil)))

;; load sql-indent
(eval-after-load "sql"
  '(load-library "sql-indent"))

;; disable evil mode in cider-repl
(evil-set-initial-state 'cider-repl-mode 'emacs)

(setq cider-prompt-save-file-on-load nil)
(setq cider-repl-result-prefix ";; => ")
(setq nrepl-hide-special-buffers t)

(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))

;; common lisp
(setq inferior-lisp-program "sbcl")
(if (file-exists-p "C:\\Users\\adam\\quicklisp\\slime-helper.el")
    (load "C:\\Users\\adam\\quicklisp\\slime-helper.el"))

;; enable `undo-tree`
(global-undo-tree-mode)

;; use `undo-tree` with evil
(evil-set-undo-system 'undo-tree)

;;;;
;; emacs general / ui settings
;;;;

(load-theme 'base16-ashes t)

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
(setq ring-bell-function #'util/friendlier-visible-bell)

;; matching parens highlight
(show-paren-mode 1)

;; global line numbers
(global-linum-mode 1)

;; no cursor blinking
(blink-cursor-mode 0)

;; full path in title bar
(setq-default frame-title-format "%b (%f)")

;; set default font height
(set-face-attribute 'default nil :height 110)

;; override custom file
(setq custom-file "~/.emacs.d/custom.el")
(if (not (file-exists-p custom-file))
    (write-region "" nil custom-file))
(load custom-file)

(add-to-list 'load-path "/usr/local/bin")
(add-to-list 'exec-path "~/bin/")

;; use aspell
(setq-default ispell-program-name "aspell")

;;;;
;; key bindings
;;;;

;; evil mode window navigation keys (mirrors my .vimrc)
(eval-after-load 'evil
  '(progn
     (global-unset-key (kbd "C-h")) ;; normally `help`
     (global-unset-key (kbd "C-l")) ;; normally `recenter-top-bottom`
     (global-unset-key (kbd "C-j")) ;; normally `paredit-newline`
     (global-unset-key (kbd "C-k")) ;; normally `paredit-kill`
     (global-set-key (kbd "C-S-h") 'help)
     (global-set-key (kbd "C-h") 'evil-window-left)
     (global-set-key (kbd "C-l") 'evil-window-right)
     (global-set-key (kbd "C-j") 'evil-window-down)
     (global-set-key (kbd "C-k") 'evil-window-up)))

;; fix format of buffer
(global-set-key (kbd "C-c f") 'util/fix-format-in-buffer)

;; comment line
(global-set-key (kbd "C-c C-/") 'util/toggle-comment-on-line)

;; magit status for the current buffer
(global-set-key (kbd "C-c g") 'magit-status)

;; clear Cider's repl buffer with Ctrl+1
(eval-after-load 'cider-repl
  '(define-key cider-repl-mode-map
     (kbd "C-1") 'cider-repl-clear-buffer))

;; toggle flyspell-mode
(global-set-key (kbd "C-c s") 'flyspell-mode)

;;;;
;; org-mode
;;;;

;; set the `org-directory` var based on the host OS
(setq org-directory (if (util/is-windows-p) "c:/Users/adam/Dropbox/org" "~/Dropbox/org"))

(defun org-file-path (filename)
  "Return the absolute address of an org file, given its relative name."
  (concat (file-name-as-directory org-directory) filename))

(setq org-inbox-file (org-file-path "inbox.org"))
(setq org-archive-location
      (concat
       (org-file-path (format "archive/archive-%s.org" (format-time-string "%Y")))
       "::* From %s"))

(setq org-refile-targets '((org-inbox-file :level . 1)
                           ((org-file-path "work-tickets.org") :level . 1)))

(setq org-agenda-files (list org-inbox-file
                             (org-file-path "work-tickets.org")))

;; record when a task is marked DONE
(setq org-log-done 'time)

;; don't allow a parent task to go to DONE items unless all children are DONE as well
(setq org-enforce-todo-dependencies t)

;;;;
;; FIXME! Things to address later.
;;;;

;; FIXME: set up some golang functionality
;;        https://github.com/hrs/dotfiles/blob/main/emacs/dot-emacs.d/configuration.org#golang
;; FIXME: set up some Python functionality
;; FIXME: convert all package installations to use `use-package`
;;        https://github.com/jwiegley/use-package
;; FIXME: Emacs C Source initialization doesn't seem to work when i set the directory
;;        Emacs var: `source-directory`
;; FIXME: write something that will set the default file coding to utf-8
;; FIXME: load specific theme for UI vs. terminal
;;        `wombat` is a good theme for terminal
;;        `base16-*` is a good theme for UI (I think I've been using base16-ashes)
;; FIXME: set the option to compile packages that are installed (? in the hrs conf file)
;; FIXME: setup an auto-timestamp when adding a new TODO task in org mode
