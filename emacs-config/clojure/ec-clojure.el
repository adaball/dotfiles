;; clojure specific

;; clojure mode
(install-package-if-missing 'clojure-mode)
(install-package-if-missing 'clojure-mode-extra-font-locking)

;; clojure mode hooks
(add-hook 'clojure-mode-hook #'paredit-mode)
(add-hook 'clojure-mode-hook #'eldoc-mode)

;; cider
(install-package-if-missing 'cider)

;; cider hooks
(add-hook 'cider-repl-mode-hook #'paredit-mode)
(add-hook 'cider-repl-mode-hook #'eldoc-mode)
(add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode)

;; cider settings and key-bindings
(setq cider-prompt-save-file-on-load nil)
(setq nrepl-hide-special-buffers t)
(setq cider-repl-result-prefix ";; => ")


(provide 'ec-clojure)