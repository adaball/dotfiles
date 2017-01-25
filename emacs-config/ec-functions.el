;; custom function

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

(defun install-package-if-missing (package)
  "perform package installation if package is missing"
  (unless (package-installed-p package)
    (package-refresh-contents)
    (package-install package)))

(defun friendlier-visible-bell ()
   "A friendlier visual bell effect. (https://www.emacswiki.org/emacs/AlarmBell#toc8)"
   (invert-face 'mode-line)
   (run-with-timer 0.1 nil 'invert-face 'mode-line))

(provide 'ec-functions)
