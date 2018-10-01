;; This file is part of Rho installation
;; (C) 2010 g/christensen

;; Emacs initialization file

;; -*- no-byte-compile: t -*-

(setq +exec-directory+ (expand-file-name (concat exec-directory "/../../../../../emacs/libexec/emacs")))

(defconst +rho-dir+ (expand-file-name (concat +exec-directory+ "/../../../..")))

(when (file-exists-p (concat +rho-dir+ "/site/.emacs.d/basic/tabbar.el"))
  (tool-bar-mode -1))

(defconst +basic-emacs?+ (file-exists-p (concat +rho-dir+ "/lite")))

(defmacro iflisp (&rest body)
  `(when (not +basic-emacs?+)
     ,@body))

(defconst +emacs-stem+ "rho")

(defconst +portable?+ (file-exists-p (concat +rho-dir+ "/../" +emacs-stem+ ".exe")))

(when +portable?+ (setenv "RHO_PORTABLE" "t"))

(defconst +precomp+ (string= (getenv "INVOCATION_TAG") "TARGET:PRECOMP"))

(add-to-list 'load-path (concat +rho-dir+ "/site/.emacs.d"))
(add-to-list 'load-path (concat +rho-dir+ "/site/.emacs.d/rho"))

(add-to-list 'load-path (concat +rho-dir+ "/bin/emacs/EmacsW32/nxhtml/util"))

(defconst +emacs-base-dir+ (concat +rho-dir+ "/site/.emacs.d"))

(load "cl-seq")

(defconst +emacsw32+ (file-exists-p (concat +rho-dir+ "/bin/emacs/EmacsW32/init.el")))
(when +emacsw32+   
  (load (expand-file-name (concat +rho-dir+ "/bin/emacs/EmacsW32/init.el"))))

(load "set-environment.el")
(load "core-customization.el")
(load "emacs-init.el")
