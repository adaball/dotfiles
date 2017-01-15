;; javascript specific

;; json mode
(install-package-if-missing 'json-mode)
(require 'json-mode)
(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))

;; js ide mode
(install-package-if-missing 'js2-mode)
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(setq js2-bounce-indent-p t)
(setq js2-basic-offset 2)

(provide 'ec-javascript)
