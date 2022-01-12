;; -*- no-byte-compile: t -*-

;; This file is a part of RHO Emacs installation
;; (C) 2021 g/christensen

;; Emacs initialization file
;; This is the site-wide Emacs initialization file.
;; Please store your personal customizations in .emacs file inside the
;; Rho Emacs home directory.

(when (or (file-exists-p (concat rho--root-dir "/lisp/base/tabbar.el"))
          (file-exists-p (concat rho--root-dir "/lisp/base/dash.el")))
  (tool-bar-mode -1))

(add-to-list 'load-path (concat rho--root-dir "/lisp/base"))

(require 's)

(setq find-program (concat "\"" rho--root-dir "/utils/gnu/find.exe\""))
(setq grep-program (concat "\"" rho--root-dir "/utils/gnu/grep.exe\""))

(add-to-list 'exec-path (concat rho--root-dir "/utils/aspell/bin"))
(custom-set-variables '(ispell-personal-dictionary (expand-file-name "~/.aspell")))

(require 'package)
(add-to-list
   'package-archives
   '("melpa" . "https://melpa.org/packages/")
   t)
(package-initialize)

;; ELPA configuration
(defconst rho--elpa-dir (concat rho--emacs-home "/elpa"))
                     
(if (not (file-exists-p rho--elpa-dir))
  (make-directory rho--elpa-dir))

(add-to-list 'load-path (concat rho--root-dir "/lisp/base/use-package"))
(require 'use-package)

(when (not rho--precomp?)
  (require 'server)
  (defun server-ensure-safe-dir (dir) t)
  (or (eq (server-running-p) t)
      (server-start)))

(rho--create-default-get-version 'aspell)
(rho--create-default-get-version 'pandoc)

;; fix org-protocol URL on Windows
(defun advice-org-protocol-check-filename (orig-fun &rest args)
  (let ((fname (car args)))
    (let ((correct-url
           (replace-regexp-in-string (regexp-quote "/?")
                                     "?" fname nil 'literal)))
      (apply orig-fun (cons correct-url (cdr args))))))
  
(advice-add 'org-protocol-check-filename-for-protocol
            :around #'advice-org-protocol-check-filename) 

(setq custom-theme-directory (concat rho--root-dir "/lisp/themes/"))
(setq custom-safe-themes t)

(add-to-list 'custom-theme-load-path (concat custom-theme-directory "/spacemacs-theme/"))
(add-to-list 'load-path (concat custom-theme-directory "/spacemacs-theme/"))

(add-to-list 'custom-theme-load-path (concat custom-theme-directory "/moe-theme/"))
(add-to-list 'load-path (concat custom-theme-directory "/moe-theme/"))
