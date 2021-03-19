;; -*- no-byte-compile: t -*-

;; This file is a part of RHO Emacs installation
;; (C) 2021 g/christensen

;; Environment setup

;; utf-8 environment
(set-language-environment "UTF-8")
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(setq system-time-locale "C")

(defun rho--replace-in-string (what with in)
  (replace-regexp-in-string (regexp-quote what) with in nil 'literal))

(defconst rho--emacs-dir (concat rho--root-dir "/bin/emacs/"))

(setenv "EMACS_DIR" rho--emacs-dir)
(setenv "EMACSDIR" rho--emacs-dir)
(setenv "RHO_DIR" rho--root-dir)

(defconst rho--home-dir (getenv "HOME"))

(if (not (file-exists-p rho--home-dir))
  (make-directory rho--home-dir))

(defconst rho--emacs-home (concat rho--home-dir "/.emacs.d"))

(if (not (file-exists-p rho--emacs-home))
  (make-directory rho--emacs-home))

;; Emacs 27 warns...
;(add-to-list 'load-path rho--emacs-home)

(setenv "USERPROFILE" rho--home-dir)
(setenv "HOME_UTF8" (encode-coding-string rho--home-dir 'utf-8))

(cd rho--home-dir)

(setenv "PATH" (concat rho--root-dir ";" (getenv "PATH")))
(setenv "PATH" (concat rho--root-dir "/bin/emacs/bin;" (getenv "PATH")))
(setenv "PATH" (concat rho--root-dir "/bin/utils;" (getenv "PATH")))
(setenv "PATH" (concat rho--root-dir "/bin/utils/gnu;" (getenv "PATH")))
(setenv "PATH" (concat rho--root-dir "/bin/utils/arc;" (getenv "PATH")))
(setenv "PATH" (concat rho--root-dir "/bin/utils/iconv/bin;" (getenv "PATH")))

(add-to-list 'exec-path (concat rho--root-dir "/bin/utils"))

(defvar rho--win-version-string (shell-command-to-string "ver"))
(string-match "\\[.*? \\([0-9][0-9]?\\)\\.\\([0-9][0-9]?\\)" rho--win-version-string)

(defconst rho--win-ver-major
          (string-to-number (match-string 1 rho--win-version-string)))
(defconst rho--win-ver-minor
          (string-to-number (match-string 2 rho--win-version-string)))
