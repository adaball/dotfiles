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

;; cider settings
(setq cider-prompt-save-file-on-load nil)
(setq nrepl-hide-special-buffers t)
(setq cider-repl-result-prefix ";; => ")

(eval-after-load 'cider-repl
  '(define-key cider-repl-mode-map
     (kbd "C-1") 'cider-repl-clear-buffer))

(provide 'ec-clojure)
