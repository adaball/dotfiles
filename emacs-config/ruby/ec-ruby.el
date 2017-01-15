;; ruby specific settings

;; enhanced ruby mode
(install-package-if-missing 'enh-ruby-mode)
(require 'enh-ruby-mode)

;; inferior ruby
(install-package-if-missing 'inf-ruby)
(require 'inf-ruby)

(add-hook 'enh-ruby-mode-hook 'inf-ruby-minor-mode)

(provide 'ec-ruby)
