;; global keybindings

;; Link win (super) key to meta
(setq x-super-keysym 'meta)

;; comment line with Ctrl-Shift-/
(global-set-key (kbd "C-?") 'toggle-comment-on-line)

;; clear cider-repl buffer
;(global-set-key (kbd "C-1") 'cider-repl-clear-buffer)

(provide 'ec-global-key-bindings)
