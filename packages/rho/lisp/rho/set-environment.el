;; -*- no-byte-compile: t; lexical-binding: t; -*-

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

(defconst rho--emacs-dir (concat rho--root-dir "/emacs/"))

(defconst rho--guest? (string= "t" (getenv "RHO_GUEST")))

;(setenv "EMACS_DIR" rho--emacs-dir)
;(setenv "EMACSDIR" rho--emacs-dir)
(setenv "RHO_DIR" rho--root-dir)

(defconst rho--home-dir (or (getenv "HOME") (expand-file-name "~")))

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

;; Rho directories go first in PATH
(setenv "PATH"
        (mapconcat #'identity
                   (append
                    (mapcar (lambda (dir) (concat rho--root-dir dir))
                            '("/utils/iconv/bin"
                              "/utils/arc"
                              "/utils/gnu"
                              "/utils"
                              "/emacs/bin"
                              ""))
                    (list (getenv "PATH")))
                   path-separator))

(add-to-list 'exec-path (concat rho--root-dir "/utils"))

;; Windows version, (0 0) when not running on Windows
(defconst rho--win-ver-major
  (if (fboundp 'w32-version) (nth 0 (w32-version)) 0))
(defconst rho--win-ver-minor
  (if (fboundp 'w32-version) (nth 1 (w32-version)) 0))
