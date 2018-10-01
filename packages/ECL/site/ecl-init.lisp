;; This file is part of Rho installation
;; (C) 2010 g/christensen

;; Clozure Common Lisp initialization script

(let ((site-path (concatenate 'string
                              (si:getenv "RHO_DIR")
                              "/site/")))
  ;(load (concatenate 'string site-path "lisp/asdf2/asdf.lisp"))
  (require "asdf")
  (load (concatenate 'string site-path "asdf-init.lisp")))

(asdf-config:asdf-setup :init-asdf-install t)

