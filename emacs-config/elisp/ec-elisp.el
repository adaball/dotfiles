;; elisp specific

;; paredit / eldoc in ielm
(add-hook 'ielm-mode-hook #'eldoc-mode)
(if (fboundp 'paredit-mode)
    (add-hook 'ielm-mode-hook #'paredit-mode))

;; paredit / eldoc in emacs lisp
(add-hook 'emacs-lisp-mode-hook #'eldoc-mode)
(if (fboundp 'paredit-mode)
    (add-hook 'emacs-lisp-mode-hook #'paredit-mode))

(provide 'ec-elisp)
