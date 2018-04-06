;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

;; .spacemacs template with comments here:
;; https://raw.githubusercontent.com/syl20bnr/spacemacs/bd7ef98e4c35fd87538dd2a81356cc83f5fd02f3/core/templates/.spacemacs.template

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-enable-lazy-installation 'unused
   dotspacemacs-ask-for-lazy-installation t
   dotspacemacs-configuration-layer-path '()
   dotspacemacs-configuration-layers
   '(
     auto-completion
     better-defaults
     clojure
     emacs-lisp
     git
     helm
     html
     javascript
     markdown
     markdown
     org
     ruby
     spell-checking
     syntax-checking
     version-control
     yaml
     )
   dotspacemacs-additional-packages
   '(
     base16-theme
     company
     wiki-summary
     )
   dotspacemacs-frozen-packages '()
   dotspacemacs-excluded-packages '()
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  (setq-default
   dotspacemacs-elpa-https t
   dotspacemacs-elpa-timeout 5
   dotspacemacs-check-for-update nil
   dotspacemacs-elpa-subdirectory nil
   dotspacemacs-editing-style 'vim
   dotspacemacs-verbose-loading nil
   dotspacemacs-startup-banner 'official
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))
   dotspacemacs-startup-buffer-responsive t
   dotspacemacs-scratch-mode 'text-mode
   dotspacemacs-themes '(base16-ocean
                         spacemacs-dark
                         spacemacs-light)
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-default-font '("Monaco"
                               :size 13
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   dotspacemacs-leader-key "SPC"
   dotspacemacs-emacs-command-key "SPC"
   dotspacemacs-ex-command-key ":"
   dotspacemacs-emacs-leader-key "M-m"
   dotspacemacs-major-mode-leader-key ","
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   dotspacemacs-distinguish-gui-tab nil
   dotspacemacs-remap-Y-to-y$ nil
   dotspacemacs-retain-visual-state-on-shift t
   dotspacemacs-visual-line-move-text nil
   dotspacemacs-ex-substitute-global nil
   dotspacemacs-default-layout-name "Default"
   dotspacemacs-display-default-layout nil
   dotspacemacs-auto-resume-layouts nil
   dotspacemacs-large-file-size 1
   dotspacemacs-auto-save-file-location 'cache
   dotspacemacs-max-rollback-slots 5
   dotspacemacs-helm-resize nil
   dotspacemacs-helm-no-header nil
   dotspacemacs-helm-position 'bottom
   dotspacemacs-helm-use-fuzzy 'always
   dotspacemacs-enable-paste-transient-state nil
   dotspacemacs-which-key-delay 0.4
   dotspacemacs-which-key-position 'bottom
   dotspacemacs-loading-progress-bar t
   dotspacemacs-fullscreen-at-startup nil
   dotspacemacs-fullscreen-use-non-native nil
   dotspacemacs-maximized-at-startup nil
   dotspacemacs-active-transparency 90
   dotspacemacs-inactive-transparency 90
   dotspacemacs-show-transient-state-title t
   dotspacemacs-show-transient-state-color-guide t
   dotspacemacs-mode-line-unicode-symbols t
   dotspacemacs-smooth-scrolling t
   dotspacemacs-line-numbers nil
   dotspacemacs-folding-method 'evil
   dotspacemacs-smartparens-strict-mode nil
   dotspacemacs-smart-closing-parenthesis nil
   dotspacemacs-highlight-delimiters 'all
   dotspacemacs-persistent-server nil
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   dotspacemacs-default-package-repository nil
   dotspacemacs-whitespace-cleanup nil
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."

  ;;;;
  ;; hooks
  ;;;;
  (add-hook 'after-init-hook 'global-company-mode)
  (add-hook 'cider-repl-mode-hook #'eldoc-mode)
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
       (define-key paredit-mode-map (kbd "<delete>")
         nil)))

  ;; disable evil mode in cider-repl
  ;; this doesn't work in Spacemacs for some reason?
  ;;(evil-set-initial-state 'cider-repl-mode 'emacs)

  ;; cider related
  (setq cider-prompt-save-file-on-load nil)
  (setq cider-repl-result-prefix ";; => ")
  (setq nrepl-hide-special-buffers t)

  ;; clear Cider's repl buffer with Ctrl+1
  (eval-after-load 'cider-repl
    '(define-key cider-repl-mode-map
       (kbd "C-1") 'cider-repl-clear-buffer))

  ;; allow for upcase region
  (put 'upcase-region 'disabled nil)
  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place your code here."
  ;;;;
  ;; utility functions
  ;;;;
  (defun util/replace-tabs ()
    "replace tabs with spaces"
    (interactive)
    (set-variable 'tab-width 2)
    (mark-whole-buffer)
    (untabify (region-beginning) (region-end))
    (keyboard-quit))

  (defun util/fix-format ()
    "re-indent entire buffer"
    (interactive)
    (mark-whole-buffer)
    (indent-region (region-beginning) (region-end))
    (keyboard-quit))

  (defun util/toggle-comment-on-line ()
    "comment or uncomment current line"
    (interactive)
    (comment-or-uncomment-region (line-beginning-position) (line-end-position)))

  (defun util/install-package-if-missing (package)
    "perform package installation if package is missing"
    (unless (package-installed-p package)
      (package-refresh-contents)
      (package-install package)))

  (defun util/friendlier-visible-bell ()
    "A friendlier visual bell effect. (https://www.emacswiki.org/emacs/AlarmBell#toc8)"
    (invert-face 'mode-line)
    (run-with-timer 0.1 nil 'invert-face 'mode-line))

  (defun util/delete-process-interactive ()
    "delete process using an ido-read buffer (http://stackoverflow.com/questions/11572934/how-do-i-kill-a-running-process-in-emacs#11573495)"
    (interactive)
    (let ((pname (ido-completing-read "Process Name: " (mapcar 'process-name (process-list)))))
      (delete-process (get-process pname))))

  (defun util/change-font-height ()
    "interactively change font height attribute"
    (interactive)
    (let ((updated-height (read-from-minibuffer (format "Enter new height (current is %d): " (face-attribute 'default :height)))))
      (set-face-attribute 'default nil :height (string-to-number updated-height))))

  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
