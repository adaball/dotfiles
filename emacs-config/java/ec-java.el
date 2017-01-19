;; java specific

(install-package-if-missing 'jdee)
(require 'jdee)

(setq jdee-server-dir "~/bin")
(setq jdee-maven-program "/usr/local/bin/mvn")
(define-key jdee-mode-map (kbd "C-c b") 'jdee-maven-build)

(provide 'ec-java)
