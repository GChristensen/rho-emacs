;; -*- no-byte-compile: t; lexical-binding: t; -*-

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
  ;; do not check the server socket directory ownership (Windows)
  (with-eval-after-load 'server
    (defun server-ensure-safe-dir (_dir) t))
  (or (eq (server-running-p) t)
      (server-start)))

(rho--create-default-get-version 'aspell)
(rho--create-default-get-version 'pandoc)

;; fix org-protocol URL on Windows
(defun rho--advice-org-protocol-check-filename (args)
  (cons (replace-regexp-in-string (regexp-quote "/?") "?" (car args) nil 'literal)
        (cdr args)))

(advice-add 'org-protocol-check-filename-for-protocol
            :filter-args #'rho--advice-org-protocol-check-filename)

(setq custom-theme-directory (expand-file-name "lisp/themes/" rho--root-dir))

;; Bundled themes are loaded without confirmation. Setting this to nil would
;; make Emacs ask for every bundled theme on first start.
(setq custom-safe-themes t)

(dolist (theme-dir '("spacemacs-theme" "moe-theme"))
  (let ((dir (expand-file-name theme-dir custom-theme-directory)))
    (add-to-list 'custom-theme-load-path dir)
    (add-to-list 'load-path dir)))
