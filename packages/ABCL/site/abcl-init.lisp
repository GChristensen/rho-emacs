;; This file is part of Rho installation
;; (C) 2010 g/christensen

;; Armed Bear Common Lisp initialization script

(load (concatenate 'string (getenv "RHO_DIR") "/site/asdf-init.lisp"))

(asdf-config:asdf-setup :init-asdf-install t)
