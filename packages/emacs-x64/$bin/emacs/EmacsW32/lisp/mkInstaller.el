;;; mkInstaller.el --- Make Inno installation script to build installer
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2004
;; Version: 0.86
;; Last-Updated: Mon Jan 15 03:08:41 2007 (3600 +0100)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; For internal use creating installers.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; History:
;;
;; 2005-12-29: Added "emacsitself".
;; 2006-06-24: Added replacesameversion flag.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This file is not part of Emacs
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; To find out more about the GNU General Public License you can visit
;; Free Software Foundation's website http://www.fsf.org/.  Or, write
;; to the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:

(defun mkInstaller-find-setup-helper-ver ()
  "Assumes postioned in new iss tmp buffer"
  (let ((pattern "AppVerName=[^\(]*(\\([^\)]*\\))")
	(ver)
	)
    (save-excursion
      (goto-char (point-min))
      (re-search-forward pattern)
      ;;(message "0=%s" (match-string 0))
      ;;(message "1=%s" (match-string 1))
      (setq ver (match-string 1))
      (let ((p (string-match " " ver)))
	(setq ver (substring ver (+ p 1)))))
    ver))

(defun mkInstaller-replace-setup-value (name newval)
  (goto-char (point-min))
  (search-forward "[Setup]")
  ;(let ((pattern (concat "^\\s*" name "=\\(.*?\\)\\n")))
  (let ((pattern (concat "^[ \\t]*" name "[ \\t]*=\\(.*\\)"))
	(dbg))
    (when dbg (message "pattern=%s" pattern))
    (when dbg (message "newval=%s" newval))
    (re-search-forward pattern)
    (when dbg (message "0=%s" (match-string 0)))
    (when dbg (message "1=%s" (match-string 1)))
    (replace-match newval t t nil 1)))

(defun mkInstaller-create-iss (&optional par-id extra-file)
  (let* ((dbg)
	 (tmpiss (expand-file-name (concat +exec-directory+ "../../EmacsW32/temp-installer.iss")))
	 (eshiss (expand-file-name (concat +exec-directory+ "../../EmacsW32/EmacsW32.iss")))
	 (s1)
	 (s2)
	 (m1 (make-marker))
	 (m2 (make-marker))
	 (sethlpver)
	 (default-output-name (format "Emacs-%d" emacs-major-version))
	 (output-name)
	 (default-display-name (format "Emacs %s" emacs-version))
	 (display-name)
	 (emacs_dir (file-name-nondirectory
		     (substring (file-name-directory (substring +exec-directory+ 0 -1)) 0 -1)))
	 (distribid
	  (if (not (equal par-id ""))
	      par-id
	    (let ((yn)
		  (bad-chars "\\/:*?\"<>|"))
	      (while (not yn)
		(setq yn (read-from-minibuffer "\nDistribution id name: "))
		(when (string-match (concat "[" bad-chars "]") yn)
		  (message "   Name must not include any of the characters %s" bad-chars)
		  (setq yn nil))
		(when (= 0 (length yn))
		  (message "   Please give a name to identify your distribution!")
		  (setq yn nil))
		)
	      (message "")
	      yn)))
	 (dist-readme-file (concat "readme-" distribid ".txt"))
	 (dist-readme-path (expand-file-name (concat +exec-directory+ "../../EmacsW32/" dist-readme-file)))
	 )
    (unless (file-exists-p dist-readme-path)
      (message "Please supply a file describing the installation package\nyou are going to create with the name:\n    ")
      (message "  %s" dist-readme-path)
      (kill-emacs 1))
    (when dbg (message "emacs_dir=%s" emacs_dir))
    (when dbg (message "eshiss=%s" eshiss))
    (when dbg (message "tmpiss=%s" tmpiss))

;;     (setq output-name
;; 	  (read-from-minibuffer (format "Setup output name (%s): " default-output-name)))
    (unless (> (length output-name) 0) (setq output-name default-output-name))
    (when dbg (message "output-name=(%s)" output-name))

;;     (setq display-name
;; 	  (read-from-minibuffer (format "Setup display name (%s): " default-display-name)))
    (unless (> (length display-name) 0) (setq display-name default-display-name))
    (when dbg (message "display-name=(%s)" display-name))


    (find-file tmpiss)
    (insert-file-contents eshiss)
    (setq sethlpver (mkInstaller-find-setup-helper-ver))
    (when dbg (message "sethlpver=%s" sethlpver))

    (setq s1 "[Messages]")
    (search-forward s1)
    (backward-char (length s1))
    (setq m1 (point))

    (setq s1 "[Setup]")
    (search-forward s1)
    (backward-char (length s1))
    (setq m2 (point))
    (delete-region m1 m2)

    (insert "[Messages]\nSetupWindowTitle=Setup - Emacs+EmacsW32 Install Wizard\n\n")
    (insert "WelcomeLabel1=Welcome to [name] Install Wizard\n\n")

    ;;(setq s1 "InfoBeforeFile=") (search-forward s1) (backward-char (length s1)) (insert ";;;")
    (mkInstaller-replace-setup-value "InfoBeforeFile" dist-readme-file)

    (mkInstaller-replace-setup-value "AppName" "Emacs+EmacsW32")
    (let ((app-ver-name (concat display-name " and EmacsW32 " sethlpver " (distribution ID: " distribid ")")))
      (mkInstaller-replace-setup-value "AppVerName" app-ver-name)
      (message "  The installer will identify itself as\n     \"%s\"." app-ver-name))
    (mkInstaller-replace-setup-value "DisableDirPage" "no")
    (mkInstaller-replace-setup-value "DisableProgramGroupPage" "no")
    (let ((outbase (concat output-name "-" distribid "-EmacsW32-" sethlpver)))
      (mkInstaller-replace-setup-value "OutputBaseFileName" outbase)
      (message "  The installer file name will be \"%s.exe\"." outbase))
    (mkInstaller-replace-setup-value "Uninstallable" "yes")
    (mkInstaller-replace-setup-value "UsePreviousAppDir" "yes")

    ;;(setq s1 "[Files]")
    (setq s1 "<<Emacs itself here>>")
    (search-forward s1)
    (insert (concat "\nSource: \"..\\" emacs_dir "\\*\"; DestDir: \"{app}\\" emacs_dir
		    "\"; Flags: recursesubdirs replacesameversion; Components: emacsitself;\n\n"))
    (when (and extra-file
               (< 0 (length extra-file)))
      (insert (concat "\nSource: \"" extra-file "\"; DestDir: \"{code:AppSubDir|setup}\"; Flags: recursesubdirs;\n\n")))

    (setq s1 "[Types]")
    (search-forward s1)
    (setq s1 "onlyunpack")
    (search-forward s1)
    (search-backward "\n")		;
    (search-forward "\n")		;
    (insert "Name: \"justemacs\"; Description: \"Install only Emacs itself\";\n")
    (insert ";;;")

    (setq s1 "[Components]")
    (search-forward s1)
    (setq s1 "Name: \"unpack\";")
    (search-forward s1)
    (search-backward "\n")		;
    (search-forward "\n")		;
    (insert "Types: full minimal justemacs; Name: \"emacsitself\"; Description: \"Emacs itself.\";\n")
    (insert ";;;")

    (setq s1 "g_bIncludesEmacs")
    (search-forward s1)
    (setq s1 "false")
    (search-forward s1)
    (delete-backward-char 5)
    (insert "true")

    (setq s1 "InternalError('g_bIncludesEmacs is true');") ;
    (search-forward s1)
    (delete-backward-char (length s1))
    (insert (concat "result := ExpandConstant('{app}\\" emacs_dir "');"))

    (message "")
    (save-buffer 0)
    ))

(if window-system (w32-send-sys-command 61488))
;;(mkInstaller-create-iss)

(provide 'mkInstaller)

;;; mkInstaller.el ends here
