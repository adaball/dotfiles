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
  (interactive (let ((new-height (read-number "Enter new height: " (amb/cur-font-height))))
                 (list new-height)))
  (set-face-attribute 'default nil :height h))

(defun amb/cur-font-height ()
  "Return the current font height value"
  (cdr (seq-first (seq-filter (lambda (v) (eq :height (car v))) (face-all-attributes 'default)))))

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

;; set font face to Source Code Pro if it's available
(when (member "Source Code Pro" (font-family-list))
  (set-face-attribute 'default nil :font "Source Code Pro"))

;; set some OS specific backups
(when (amb/is-windows-p) (set-fontset-font t nil "Consolas" nil 'append))
(when (amb/is-macos-p) (set-fontset-font t nil "Monaco" nil 'append))

;; set Emacs C source directory on home desktop
(if (and (amb/is-windows-p) (file-exists-p "c:/Users/adam/bin/emacs-27.2-src/"))
    (setq source-directory "c:/Users/adam/bin/emacs-27.2-src/"))

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

(use-package cider
  :config 
  (setq cider-prompt-save-file-on-load nil)
  (setq cider-repl-result-prefix ";; => ")
  (setq nrepl-hide-special-buffers t)
  (add-hook 'cider-repl-mode-map #'eldoc-mode)
  :bind (:map cider-repl-mode-map
              ("C-1" . cider-repl-clear-buffer))
  :ensure t)

(use-package clhs
  :ensure t)

(use-package clojure-mode
  :config
  (add-hook 'clojure-mode-hook #'eldoc-mode)
  :ensure t)

(use-package clojure-mode-extra-font-locking
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
  (evil-set-undo-system 'undo-tree)
  (global-set-key (kbd "C-S-h") 'help)
  (global-set-key (kbd "C-?") 'comment-or-uncomment-region)
  (add-hook 'package-menu-mode-hook #'turn-on-evil-mode)

  ;; for `M-x list-packages`
  ;; https://www.reddit.com/r/emacs/comments/7dsm0j/comment/dq03zwu/
  (add-to-list 'evil-buffer-regexps '("*Packages*" . normal))
  (with-eval-after-load 'package
    ;; movement keys j,k,l,h set up for free by defaulting to normal mode.
    ;; mark, unmark, install
    (evil-define-key 'normal package-menu-mode-map (kbd "m") #'package-menu-mark-install)
    (evil-define-key 'normal package-menu-mode-map (kbd "u") #'package-menu-mark-unmark)
    (evil-define-key 'normal package-menu-mode-map (kbd "x") #'package-menu-execute))
  :demand t
  :ensure t)

(use-package evil-org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda () (evil-org-set-key-theme)))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)
  :ensure t)

(use-package json-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.json$" . json-mode))
  :ensure t)

(use-package lispy
  :ensure t)

(use-package magit
  :bind ("C-c g" . magit-status)
  :ensure t)

(use-package markdown-mode
  :ensure t)

(use-package org
  :config
  ;; global org capture
  (global-set-key (kbd "C-c c") #'org-capture)

  ;; open a file in the defined `org-directory` var
  (global-set-key (kbd "C-x f") ;; previously `set-fill-column`
                  '(lambda ()
                     (interactive)
                     (find-file
                      (read-file-name "Org file: "
                                      ;; make sure it ends in a "/"
                                      (if (string= "/" (substring gtd-directory -1))
                                          gtd-directory
                                        (concat gtd-directory "/"))))))

  ;; insert an inactive org time stamp
  (global-set-key (kbd "C-x t") ;; not previously bound AFAICT
                  '(lambda ()
                     (interactive)
                     (org-time-stamp-inactive (format-time-string "%H:%m"))))

  (defun org-export-output-file-name-modified (orig-fun extension &optional subtreep pub-dir)
    "Set org exporting to a specified directory (https://stackoverflow.com/a/47850858)"
    (unless pub-dir
      (setq pub-dir (concat (file-name-as-directory org-directory) "exported-org-files"))
      (unless (file-directory-p pub-dir)
        (make-directory pub-dir)))
    (apply orig-fun extension subtreep pub-dir nil))

  ;; export org files to a specific directory in ORG-DIRECTORY
  (advice-add 'org-export-output-file-name :around #'org-export-output-file-name-modified)

  ;; record when a task is marked DONE
  (setq org-log-done 'time)

  ;; don't allow a parent task to go to DONE items unless all children are DONE as well
  (setq org-enforce-todo-dependencies t)

  ;; set the ORG-DIRECTORY var based on the host OS
  (setq org-directory (if (amb/is-windows-p) "c:/Users/adam/Dropbox/org" "~/Dropbox/org"))

  ;; set the GTD-DIRECTORY var based on ORG-DIRECTORY
  (setq gtd-directory (concat (file-name-as-directory org-directory) "gtd"))

  (defun gtd-file-path (filename)
    "Given FILENAME, return the system specific GTD path to it"
    (concat (file-name-as-directory gtd-directory) filename))

  ;; gtd org files
  (setq inbox-file (gtd-file-path "inbox.org"))
  (setq next-actions-file (gtd-file-path "next-actions.org"))
  (setq projects-file (gtd-file-path "projects.org"))
  (setq reference-file (gtd-file-path "reference.org"))
  (setq someday-file (gtd-file-path "someday.org"))
  (setq trash-file (gtd-file-path "trash.org"))
  (setq waiting-file (gtd-file-path "waiting.org"))

  ;; set archive location in the gtd directory, based on YYYY
  (setq org-archive-location
        (concat
         (gtd-file-path (format "archive-%s.org" (format-time-string "%Y")))
         "::* From %s"))

  (setq org-agenda-files (list
                          inbox-file
                          next-actions-file
                          projects-file
                          reference-file
                          someday-file
                          trash-file
                          waiting-file))

  (setq org-capture-templates '(("i" "Inbox" entry (file+headline inbox-file "Inbox")
                                 "* TODO %i%? \n  %U")
                                ("n" "Next Actions" entry (file+headline next-actions-file "Next Actions")
                                 "* TODO %i%? \n  %U")
                                ("p" "Projects" entry (file+headline projects-file "Projects")
                                 "* TODO %i%? \n  %U")
                                ("s" "Someday" entry (file+headline someday-file "Someday")
                                 "* TODO %i%? \n  %U")
                                ("w" "Waiting" entry (file+headline waiting-file "Waiting")
                                 "* TODO %i%? \n  %U")))

  (setq org-refile-targets '((inbox-file :maxlevel . 1)
                             (next-actions-file :maxlevel . 1)
                             (projects-file :maxlevel . 1)
                             (reference-file :maxlevel . 1)
                             (someday-file :maxlevel . 1)
                             (trash-file :maxlevel . 1)
                             (waiting-file :maxlevel . 1)))

  (setq org-refile-allow-creating-parent-nodes t)
  (setq org-refile-use-outline-path "file"))

(use-package paredit
  :bind (:map paredit-mode-map
              ("<delete>" . nil))
  :config
  (add-hook 'cider-repl-mode-hook #'enable-paredit-mode)
  (add-hook 'clojure-repl-mode-hook #'enable-paredit-mode)
  (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'ielm-mode-hook #'enable-paredit-mode)
  :ensure t)

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode)
  :ensure t)

(use-package slime
  :config
  (setq inferior-lisp-program "sbcl")
  (if (file-exists-p "C:\\Users\\adam\\quicklisp\\slime-helper.el")
      (load "C:\\Users\\adam\\quicklisp\\slime-helper.el"))
  :ensure t)

(use-package sql-indent
  :config
  (add-hook 'sql-mode-hook #'sqlind-minor-mode)
  :ensure t)

(use-package undo-tree
  :bind (:map undo-tree-map
              ("C-?" . nil))
  :config
  (global-undo-tree-mode)
  :ensure t)

(use-package vlf
  :ensure t)

(use-package wiki-summary
  :ensure t)

;;;;
;; requires / hooks / init
;;;;

(require 'uniquify)

(setq uniquify-buffer-name-style 'forward)
(add-hook 'emacs-lisp-mode-hook #'eldoc-mode)
(add-hook 'ielm-mode-hook #'eldoc-mode)

;;;;
;; emacs general / ui settings
;;;;

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

;; use aspell
(setq-default ispell-program-name "aspell")

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

;;;;
;; FIXME! Things to address later.
;;;;

;; FIXME: set up some golang functionality
;;        https://github.com/hrs/dotfiles/blob/main/emacs/dot-emacs.d/configuration.org#golang
;; FIXME: set up some Python functionality
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
;; FIXME: figure out how to make the FIND-DONE prefix argument the default when arching org subtress (i.e. only ever archive DONE items)
;; FIXME: the evil bindings (and all bindings in general) seem to be messed up when using the terminal
;;        for instance, I was unable to use `M-x b` to toggle buffers while in a terminal
;; FIXME: linking a subtree to another subtree (e.g. refile-copy from projects to next actions, but 
;;        having a link to the project from the next actions file)
