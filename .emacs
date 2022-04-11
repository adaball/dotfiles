;; emacs config file

;;;;
;; initialize package installation logic
;;;;

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

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
  (interactive "sEnter new height: ")
  (set-face-attribute 'default nil :height (if (eq (type-of h) 'string) (string-to-number h) h)))

(defun amb/is-windows-p ()
  "Confirm if the running OS is Windows"
  (string= system-type "windows-nt"))

(defun amb/is-macos-p ()
  "Confirm if the running OS is macOS"
  (string= system-type "darwin"))

(defun amb/url-escape-region (start end)
  "URL escape the current selected text"
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
      (linum-mode)
      (org-mode)
      (undo-tree-mode))
    (if switch (switch-to-buffer buffer) buffer)))

;;;;
;; OS specific
;;;;

;; set font face to Source Code Pro
(set-face-attribute 'default nil :font "Source Code Pro")

;; set Emacs C source directory on home desktop
(if (and (amb/is-windows-p) (file-exists-p "c:/Users/adam/bin/emacs-27.2-src/"))
    (setq source-directory "c:/Users/adam/bin/emacs-27.2-src/"))

;;;;
;; miscellaneous, pre-package load
;;;;

;; disable deprecation warnings about the `cl` library
(setq byte-compile-warnings '(cl-functions))

;;;;
;; install packages
;;;;

(amb/install-package-if-missing 'base16-theme)
(amb/install-package-if-missing 'cider)
(amb/install-package-if-missing 'clhs)
(amb/install-package-if-missing 'clojure-mode)
(amb/install-package-if-missing 'clojure-mode-extra-font-locking)
(amb/install-package-if-missing 'company)
(amb/install-package-if-missing 'evil)
(amb/install-package-if-missing 'json-mode)
(amb/install-package-if-missing 'magit)
(amb/install-package-if-missing 'markdown-mode)
(amb/install-package-if-missing 'midje-mode)
(amb/install-package-if-missing 'paredit)
(amb/install-package-if-missing 'rainbow-delimiters)
(amb/install-package-if-missing 'slime)
(amb/install-package-if-missing 'sql-indent)
(amb/install-package-if-missing 'undo-tree)
(amb/install-package-if-missing 'vlf)
(amb/install-package-if-missing 'wiki-summary)

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

;; (load-theme 'base16-ashes t)
(load-theme 'wombat t)

;; don't use backup files
(setq make-backup-files nil)

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
(custom-set-faces
 '(hl-line ((t (:background "#393f45")))))
(global-hl-line-mode 1)

;; use friendlier visual bell
(setq visible-bell nil)
(setq ring-bell-function #'amb/friendlier-visible-bell)

;; matching parens highlight
(show-paren-mode 1)

;; global line numbers
(setq linum-format " %03d")
(global-linum-mode 1)

;; no cursor blinking
(blink-cursor-mode 0)

;; full path in title bar
(setq-default frame-title-format "%b (%f)")

;; set default font height
(amb/set-font-height 120)

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
(global-set-key (kbd "C-c f") 'amb/fix-format-in-buffer)

;; comment line
(global-set-key (kbd "C-c C-/") 'amb/toggle-comment-on-line)

;; magit status for the current buffer
(global-set-key (kbd "C-c g") 'magit-status)

;; clear Cider's repl buffer with Ctrl+1
(eval-after-load 'cider-repl
  '(define-key cider-repl-mode-map
     (kbd "C-1") 'cider-repl-clear-buffer))

;; toggle flyspell-mode
(global-set-key (kbd "C-c s") 'flyspell-mode)

;; view the yank menu
(global-set-key (kbd "C-c y")
                '(lambda ()
                   (interactive)
                   (popup-menu 'yank-menu)))

;;;;
;; org-mode
;;;;

;; set the `org-directory` var based on the host OS
(setq org-directory (if (amb/is-windows-p) "c:/Users/adam/Dropbox/org" "~/Dropbox/org"))

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

;; set exporting to a specific directory
;; https://stackoverflow.com/a/47850858
(defun org-export-output-file-name-modified (orig-fun extension &optional subtreep pub-dir)
  (unless pub-dir
    (setq pub-dir (concat (file-name-as-directory org-directory) "exported-org-files"))
    (unless (file-directory-p pub-dir)
      (make-directory pub-dir)))
  (apply orig-fun extension subtreep pub-dir nil))
(advice-add 'org-export-output-file-name :around #'org-export-output-file-name-modified)

;; open a file in the defined `org-directory` var
(global-set-key (kbd "C-x f") ;; previously `set-fill-column`
                '(lambda ()
                   (interactive)
                   (find-file
                    (read-file-name "Org file: "
                                    ;; make sure it ends in a "/"
                                    (if (string= "/" (substring org-directory -1))
                                        org-directory
                                      (concat org-directory "/"))))))

;; insert an inactive org time stamp
(global-set-key (kbd "C-x t") ;; not previously bound AFAICT
                '(lambda ()
                   (interactive)
                   (org-time-stamp-inactive (format-time-string "%H:%m"))))

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
;; FIXME: add exception handling to convert-epoch util function (shouldn't erase region unless conversion is successful)
;; FIXME: `C-j` should behave differently when in evil's insert mode vs. normal mode: in insert mode it should
;;        move the cursor to the next line, in normal mode it should go to the window below the current one
;; FIXME: figure out a better key pattern for my custom binds (is `C-x` really the best choice?)
