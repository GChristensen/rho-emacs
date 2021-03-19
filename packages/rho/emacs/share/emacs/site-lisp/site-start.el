;; This file is a part of RHO Emacs installation
;; (C) 2021 g/christensen

;; Emacs initialization file

;; -*- no-byte-compile: t -*-

(setq rho--exec-dir (expand-file-name (concat exec-directory "/../../../../../emacs/libexec/emacs")))

(defconst rho--root-dir (expand-file-name (concat rho--exec-dir "/../../..")))

(defconst rho--name "rho")

(defconst rho--portable? (file-exists-p (concat rho--root-dir "/home")))

(when rho--portable? (setenv "RHO_PORTABLE" "t"))

(defconst rho--precomp? (string= (getenv "INVOCATION_TAG") "TARGET:PRECOMP"))

(defconst rho--lisp-dir (concat rho--root-dir "/lisp/"))

(add-to-list 'load-path rho--lisp-dir)
(add-to-list 'load-path (concat rho--lisp-dir "rho"))


(load "set-environment.el")
(load "core-customization.el")
(load "emacs-init.el")
