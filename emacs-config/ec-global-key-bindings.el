;; global keybindings

;; Link win (super) key to meta
(setq x-super-keysym 'meta)

;; comment line with Ctrl-Shift-/
(global-set-key (kbd "C-?") 'toggle-comment-on-line)

(provide 'ec-global-key-bindings)
