;; misc package imports

;; paredit
(install-package-if-missing 'paredit)
(require 'paredit)

;; magit
(install-package-if-missing 'magit)
(require 'magit)

;; rainbows!
(install-package-if-missing 'rainbow-delimiters)
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; markdown mode
(install-package-if-missing 'markdown-mode)
(require 'markdown-mode)

;; vimrc mode
(install-package-if-missing 'vimrc-mode)
(require 'vimrc-mode)

;; web mode
(install-package-if-missing 'web-mode)
(require 'web-mode)
(setq web-mode-code-indent-offset 2)

;; regex builder
(require 're-builder)
(setq reb-re-syntax 'string)

;; company mode
(install-package-if-missing 'company)
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

;; base-16 themes
(install-package-if-missing 'base16-theme)
(load-theme 'base-16-ashes t)

(provide 'ec-misc-imports)
