;; This file is part of Rho installation
;; (C) 2010 g/christensen

;; General Rho Common lisp setup code

(defpackage :rho
  (:use :cl)
  (:export :*rho-dir*
           :*home-dir*
           :load-quicklisp
            ))

(in-package :rho)

(defvar *rho-dir* (asdf-config:get-env-var "RHO_DIR"))
(defvar *home-dir* (asdf-config:get-env-var "HOME"))

(defun load-quicklisp ()
  (load (concatenate 'string *rho-dir* "/site/quicklisp-init.lisp"))
  (values))