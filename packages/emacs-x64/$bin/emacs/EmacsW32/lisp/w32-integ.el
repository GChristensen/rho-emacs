;;; w32-integ.el --- W32 specific, integration with Windows Explorer and shell.

;; Copyright (C) 2005, 2006, 2007 by Lennart Borgman

;; Author: Lennart Borgman
;; Created: 2004-07-31
;; Version: 0.81
;; Last-Updated: Mon Jan 15 03:09:16 2007 (3600 +0100)
;; Keywords: convenience processes w32
;; URL: http://OurComments.org/Emacs/DL/elisp/w32-integ.el


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; w32-integ.el contains some support for integration of Emacs with MS
;; Windows.
;;
;; To use this module put it in Emacs load-path and write in your .emacs:
;;    (require 'w32-integ)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; History:
;;
;; 2005-12-28 Removed code that adds this to dired hook on just load.


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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(if (not (eq system-type 'windows-nt))

    (error "w32-integ.el can only be used on ms windows")

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;;;; w32-shell-execute enhanced with messages

  (defcustom w32-shell-execute-lock-wait 5
    "Seconds to wait before lock message after calling ShellExecute."
    :group 'w32)

  (defun w32-shell-execute-with-msg (verb file-or-url-name
					 &optional parameters show-flag)
    "Call w32-shell-execute and give a message on error.
Show also a waiting message in the bottom of the screen."
    (let* ((obj-name (if (file-exists-p file-or-url-name)
			 (convert-standard-filename file-or-url-name)
		       file-or-url-name))
	   (msg (format "Asked OS to %s %s  ..." verb obj-name))
	   (tmsg (concat msg " (locked? popup minimized?)"))
	   (tlock (run-with-timer w32-shell-execute-lock-wait nil 'message tmsg))
	   )
      (condition-case err
	  (progn
	    (message msg)
	    (w32-shell-execute verb obj-name parameters (if show-flag show-flag 1))
	    (cancel-timer tlock)
	    (run-with-timer 9 nil (lambda () (message "")))
	    )
	(error (let ((msg (error-message-string err)))
		 (cancel-timer tlock)
		 ;;(message "orig err: %s" msg)
		 (setq msg (replace-regexp-in-string "ShellExecute failed: " "" msg))
		 (setq msg (replace-regexp-in-string "[\r\n ]*$" "" msg))
		 (setq msg (replace-regexp-in-string "this operation"
						     (concat "'" verb "'") msg))
		 (message msg))))))




  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;; Dired integration with Windows Explorer
  ;; From EmacsWiki, after some ideas from Patrik Anderson and others


  (defun w32-integ-dired-execute (verb &optional show-flag)
    "Call ShellExecute with current file from dired."
    ;;(let ((file-name (dired-get-file-for-visit)))
    (let ((file-name (dired-get-filename)))
      (w32-shell-execute-with-msg verb file-name nil show-flag)))

  (defun w32-integ-dired-explorer-open ()
    "Open current file from dired by calling Windows Explorer."
    (interactive)
    (w32-integ-dired-execute "open"))


  (defun w32-integ-dired-explorer-print ()
    "Print current file from dired by calling Windows Explorer."
    (interactive)
    (w32-integ-dired-execute "print"))


  (defcustom w32-integ-dired-key-explorer-open "\"\C-co\""
    "Dired key that will open the selected file in Windows Explorer."
    :type 'string
    :group 'w32)

  (defcustom w32-integ-dired-key-explorer-print "\"\C-cp\""
    "Dired key that will print the selected file with Windows Explorer."
    :type 'string
    :group 'w32)

  (defun w32-integ-dired-hook ()
    "Setup keys for dired-mode."
    (eval (read (concat "(define-key dired-mode-map "
			w32-integ-dired-key-explorer-open
			"'w32-integ-dired-explorer-open)")))
    (eval (read (concat "(define-key dired-mode-map "
			w32-integ-dired-key-explorer-print
			"'w32-integ-dired-explorer-print)")))
    )

  (defun w32-integ-add-dired-hook ()
    (add-hook 'dired-load-hook 'w32-integ-dired-hook))
  )

(provide 'w32-integ)

;;; w32-integ.el ends here
