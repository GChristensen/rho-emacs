;; This file is part of Rho installation
;; (C) 2010 g/christensen

;; CLISP initialization script

(let ((site-path (concatenate 'string
                              (getenv "RHO_DIR")
                              "/site/")))
  (load (concatenate 'string site-path "lisp/asdf2/asdf.lisp"))
  (load (concatenate 'string site-path "asdf-init.lisp")))

(asdf-config:asdf-setup :init-asdf-install t)