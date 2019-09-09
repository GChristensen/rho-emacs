;; -*- no-byte-compile: t -*-

;; This file is part of Rho installation
;; (C) 2010 g/christensen

;; Emacs initialization file
;; This is the site-wide Emacs initialization file.
;; Please store your personal customizations in .emacs file inside the
;; Rho Emacs home directory.

(require 's)

(add-to-list 'load-path (concat +rho-dir+ "/site/.emacs.d/basic"))

(setq find-program 
      (concat +rho-dir+ "/bin/emacs/EmacsW32/gnuwin32/bin/find.exe"))
(setq grep-program 
      (concat +rho-dir+ "/bin/emacs/EmacsW32/gnuwin32/bin/grep.exe"))

(add-to-list 'exec-path (concat +rho-dir+ "/bin/utils/aspell/bin"))
(custom-set-variables '(ispell-personal-dictionary (expand-file-name "~/.aspell")))

(require 'package)
(add-to-list
   'package-archives
   '("melpa" . "http://melpa.milkbox.net/packages/")
   t)
(package-initialize)

(add-to-list 'load-path (concat +rho-dir+ "/site/.emacs.d/basic/use-package"))
(require 'use-package)

(when (not +precomp+)
  (require 'server)
  (defun server-ensure-safe-dir (dir) t)

  ;; set up magical EmacsW32 emacsclient
  (when (string= "yes" (getenv "EMACSCLIENT_STARTING_SERVER"))
    (let ((server-file (getenv "EMACS_SERVER_FILE")))
      (when server-file
        (setq server-auth-dir (file-name-directory server-file)
              server-name (file-name-nondirectory server-file)))))

  (or (eq (server-running-p) t)
      (server-start)))

(setq custom-theme-directory (concat +rho-dir+ "/site/.emacs.d/themes/"))
(setq custom-safe-themes t)

(add-to-list 'custom-theme-load-path (concat custom-theme-directory "/moe-theme/"))
(add-to-list 'load-path (concat custom-theme-directory "/moe-theme/"))

(add-to-list 'custom-theme-load-path (concat custom-theme-directory "/spacemacs-theme/"))
(add-to-list 'load-path (concat custom-theme-directory "/spacemacs-theme/"))
