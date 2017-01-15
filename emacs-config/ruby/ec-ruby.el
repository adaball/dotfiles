;; ruby specific settings

(defun ruby-send-buffer ()
    "send whole buffer to inf-ruby"
  (interactive)
  (mark-whole-buffer)
  (ruby-send-region-and-go (region-beginning) (region-end))
  (keyboard-quit))

;; enhanced ruby mode
(install-package-if-missing 'enh-ruby-mode)
(require 'enh-ruby-mode)
(add-to-list 'auto-mode-alist '("\\.rb" . enh-ruby-mode))

;; inferior ruby
(install-package-if-missing 'inf-ruby)
(require 'inf-ruby)

(add-hook 'enh-ruby-mode-hook 'inf-ruby-minor-mode)

(eval-after-load 'inf-ruby
  '(define-key inf-ruby-minor-mode-map
     (kbd "C-c C-s") 'ruby-send-buffer))

(provide 'ec-ruby)
