;; -*- no-byte-compile: t -*-

;; This file is part of Rho installation
;; (C) 2010 g/christensen

;; Environment setup

;; utf-8 environment
(set-language-environment "UTF-8")
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Little hack to get proper Windows cyrillic language environment setup
;; out of the box
(defconst +lang+ (getenv "LANG"))

;; (when (string= "rus" (downcase +lang+))
;;   (defadvice locale-name-match (around locale-name-match-cyr-hack activate)
;;     (if (string= "rus" (downcase key))
;;         (cond ((eq alist locale-language-names)
;;                (setq ad-return-value '("Russian" windows-1251)))
;;               ((eq alist locale-preferred-coding-systems)
;;                (setq ad-return-value 'windows-1251)))
;;         ad-do-it))
;;   (set-locale-environment)

;;   (defadvice shell (around shell-coding-system-override activate)
;;     (let* ((buffer ad-do-it)
;; 	   (process (get-buffer-process buffer)))
;;       (set-process-coding-system process 'windows-1251-dos 'windows-1251-dos)
;;       (setq ad-return-value buffer)))

;;   (setq default-process-coding-system '(windows-1251-dos . windows-1251-dos)))

;; Set environment variables
;(load "alien-bind.el")

(defconst +emacs-dir+ (concat +rho-dir+ "/bin/emacs/"))
(defconst +rho-dir-win-path+ (substitute ?\\ ?/ +rho-dir+))
(defconst +rho-dir-unix-path+ (substitute ?/ ?\\ +rho-dir+))

(setenv "EMACS_DIR" +emacs-dir+)
(setenv "EMACSDIR" +emacs-dir+)
(setenv "RHO_DIR" +rho-dir+)

;; (alien-bind (javaHome redist2008 browseCommand)
;;   'vbs "
;;          On Error Resume Next
;;          Dim objShell
;;          Dim browseCommand
;;          Dim javaHome

;;          Set objShell = CreateObject(\"WScript.Shell\")

;;          ' Get JDK or JRE installation dir.

;;          Dim javaVer 
;;          javaVer = objShell.RegRead(\"HKLM\\SOFTWARE\\JavaSoft\\Java Development Kit\\CurrentVersion\")
;;          javaHome = objShell.RegRead(\"HKLM\\SOFTWARE\\JavaSoft\\Java Development Kit\\\" & javaVer & \"\\JavaHome\")

;;          If javaHome = \"\" Then
;;            javaVer = objShell.RegRead(\"HKLM\\SOFTWARE\\JavaSoft\\Java Runtime Environment\\CurrentVersion\")
;;            javaHome = objShell.RegRead(\"HKLM\\SOFTWARE\\JavaSoft\\Java Runtime Environment\\\" & javaVer & \"\\JavaHome\")
;;          End If

;;          ' MSVC++ 2008 Redistributable
;;          redist2008 = objShell.RegRead(\"HKLM\\SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\Uninstall\\{FF66E9F6-83E7-3A3E-AF14-8DE9A809A6A4}\\VersionMajor\") = \"9\"
     
;;          ' Get default browser path
;;          browseCommand = objShell.RegRead(\"HKCR\\http\\shell\\open\\command\\\")"
;;   (if (boundp 'browseCommand)
;;       (progn
;;         (let ((private-jdk (concat +rho-dir+ "/bin/jdk")))
;;           (defconst +java-home+ 
;;             (if (or (file-exists-p private-jdk) (not javaHome) (string= javaHome ""))
;;                 private-jdk javaHome)))
;;         (defconst +redist-2008+ (string= redist2008 "True"))
;;         (defconst +browse-command+ browseCommand))
;;     (progn
;;         (defconst +java-home+ (concat +rho-dir+ "/bin/jdk"))
;;         (defconst +redist-2008+ nil)
;;         (defconst +browse-command+ "iexplore %1")
;;         (when (not (string= (getenv "INVOCATION_TAG") "TARGET:PRECOMP"))
;;           (display-warning '(rho)
;;                            "Windows Scripting Host command-line utility (cscript) is disabled or not available,
;; this could impact the functionality of some Rho components.
;; Please enable Windows Scripting Host or make it available.")))))

(require 'w32-reg-iface)

(defconst +java-home+ (concat +rho-dir+ "/bin/jdk"))
(defconst +redist-2008+ nil)
(defconst +browse-command+ (car (w32-reg-iface-read-value "HKCR/http/shell/open/command/")))

(defconst +home-dir+ (getenv "HOME"))
(defconst +home-dir-unix-path+ (substitute ?/ ?\\ +home-dir+))
(defconst +anyhome?+ (getenv "ANYHOME"))

(if (not (file-exists-p +home-dir+))
  (make-directory +home-dir+))

(defconst +emacs-home+ (concat +home-dir+ "/.emacs.d"))

(if (not (file-exists-p +emacs-home+))
  (make-directory +emacs-home+))

;(add-to-list 'load-path +emacs-home+)

(setenv "USERPROFILE" +home-dir+)
(setenv "HOME_UTF8" (encode-coding-string +home-dir+ 'utf-8))

(cd +home-dir+)

(setenv "PATH" (concat +rho-dir+ ";" (getenv "PATH")))
(setenv "PATH" (concat +rho-dir+ "/site;" (getenv "PATH")))
(setenv "PATH" (concat +rho-dir+ "/batch;" (getenv "PATH")))
(setenv "PATH" (concat +rho-dir+ "/bin/emacs/bin;" (getenv "PATH")))
(setenv "PATH" (concat +rho-dir+ "/bin/emacs/EmacsW32;" (getenv "PATH")))
(setenv "PATH" (concat +rho-dir+ "/bin/emacs/EmacsW32/bin;" (getenv "PATH")))
(setenv "PATH" (concat +rho-dir+ "/bin/emacs/EmacsW32/gnuwin32/bin;" (getenv "PATH")))
(setenv "PATH" (concat +rho-dir+ "/bin/utils;" (getenv "PATH")))
(add-to-list 'exec-path (concat +rho-dir+ "/bin/emacs/EmacsW32"))
(add-to-list 'exec-path (concat +rho-dir+ "/bin/emacs/EmacsW32/bin"))
(add-to-list 'exec-path (concat +rho-dir+ "/bin/emacs/EmacsW32/gnuwin32/bin"))
(add-to-list 'exec-path (concat +rho-dir+ "/bin/utils"))

(defvar *win-version-string* (shell-command-to-string "ver"))
(string-match "\\[.*? \\([0-9][0-9]?\\)\\.\\([0-9][0-9]?\\)" *win-version-string*)

(defconst +win-ver-major+ 
          (string-to-number (match-string 1 *win-version-string*)))
(defconst +win-ver-minor+ 
          (string-to-number (match-string 2 *win-version-string*)))

(iflisp

(defconst +lisp-dir+ (concat +home-dir+ "/lisp"))
(if (and (not +anyhome?+) (not (file-exists-p +lisp-dir+)))
  (make-directory +lisp-dir+))

;; lispx-proxy initialization
(defconst +lispx-dir+ (concat +home-dir+ "/.lispx"))
(if (and (not +anyhome?+) (not (file-exists-p +lispx-dir+)))
  (make-directory +lispx-dir+))

(defun regular-directory-p (dir)
  (and (file-directory-p dir)
	   (not (or (string-match "\\.\\." dir)
				(string-match "\\." dir)))))

(defun get-lispx-app-private-path (root-dir)
  (when (regular-directory-p root-dir)
	(if (file-exists-p (concat root-dir "/.noindex"))
		(list root-dir)
	    (cons root-dir 
			  (mapcan #'get-lispx-app-private-path
					  (directory-files root-dir t))))))

;; add lisp applications `shared' subdirectory subtree to PATH env. variable
(when (not +anyhome?+) 
  (setenv "PATH" 
        (concat 
         (mapconcat 
           'identity
		   (mapcan
			(lambda (dir)
			  (when (file-exists-p (concat dir "/application.meta"))
				(let ((shared-dir (concat dir "/shared")))
				  (when (file-exists-p shared-dir)
					(get-lispx-app-private-path shared-dir)))))
			(directory-files (concat +home-dir+ "/lisp") t))
		   ";") ";"
         (getenv "PATH"))))

) ; iflisp

