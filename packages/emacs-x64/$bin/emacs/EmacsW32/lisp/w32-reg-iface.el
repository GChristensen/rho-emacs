;;;  w32-reg-iface.el -- Interface to the registry, using external process.

;; Copyright (C) 2004, 2005, 2006, 2007 by Lennart Borgman and Steve Kemp

;; Original Author: Steve Kemp <skx@tardis.ed.ac.uk>
;; Rewritten by: Lennart Borgman (lennart O borgman A gmail O com)
;; Version: 0.82
;; Last-Updated: 2008-03-06T01:23:10+0100 Thu

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This module allows access to the MS Windows Registry from Emacs through the use of an external program.
;; It only allows reading from the Registry.
;;
;; This interface module will provide the five functions that the
;; registry mode relies upon the most:
;;
;;   Checking for Key Existence.
;;   Enumerating Keys
;;   Enumerating Values
;;   Finding the Root Keys.
;;   Reading Values
;;
;;
;; Installing this interface, and using it to access the registry
;; should be fairly simple:
;;
;; Just copy w32-reg-iface.exe to a directory on your PATH (it may
;; also be in setup\bin), and place the following line in your .emacs
;; file:
;;
;;   ;; Load the registry interface
;;   (require 'w32-reg-iface)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;;  Limitations and bugs
;;
;; If a key or value has the same name as the end marker all values
;; will not be read.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; History:
;;
;; 2004: Lennart Borgman
;;     - Bug fixes.
;;     - Added a slightly more reliable end marker.
;;     - Added test of key existence.
;;     - Reading key-value pair gives nil if key-value does not exists.
;;     - Added a defcustom for directory path of external program.
;;     - Added functions to search first HKCU and then HKLM, named -culm-.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(defvar w32-reg-iface-prog-name "w32-reg-iface.exe"
  "Name of the registry interface program.")

(defcustom w32-reg-iface-prog-dir ""
  "Directoy where w32-reg-iface.exe is located.
If the program is not in your PATH or in ../../EmacsW32/bin then you
should specify its location directory completly here - or move it into
a directory that is contained on your path."
  :type 'string
  :group 'w32)

(defconst w32-reg-iface-dir (file-name-directory
                             (if load-file-name load-file-name buffer-file-name)))

;;(expand-file-name "../bin" w32-reg-iface-dir)
(defun w32-reg-iface-prog ()
  "File name to use when calling w32-reg-iface.exe."
  (if (= 0 (length w32-reg-iface-prog-dir))
      (if (executable-find w32-reg-iface-prog-name)
	  w32-reg-iface-prog-name
	(convert-standard-filename
         (expand-file-name
          w32-reg-iface-prog-name
          ;;(expand-file-name "../../EmacsW32/bin/" +exec-directory+)
          (expand-file-name "../bin" w32-reg-iface-dir)
          )))
    (convert-standard-filename
     (expand-file-name
      (concat (file-name-as-directory w32-reg-iface-prog-dir)
	      w32-reg-iface-prog-name)))))


(defun w32-reg-iface-slash2b (orig-name)
  (if (string-match "\\\\" orig-name)
      orig-name
    (let ((start 0)
          (name orig-name))
      (while (string-match "/" name start)
        (aset name (match-beginning 0) ?\\)
        (setq start (match-end 0)))
      name)))
;;(w32-reg-iface-slash2b "SOFTWARE\\Cygnus Solutions\\Cygwin\\mounts v2\\/\\native")


(defun w32-reg-iface-get-root-keys ()
  "Return a list of the registry root keys."
  (interactive)
  (with-temp-buffer
    (let ((buffer (current-buffer))
	  (results nil)
          (default-directory +exec-directory+)
	  (finished nil))
      (call-process (w32-reg-iface-prog) nil buffer nil
		    "--root")
      ;;(message "%s" (buffer-string))
      (goto-char 0)
      (while (not finished)
	(re-search-forward "^\\(.+\\)$")
	(if (equal ";;CMNT -end-mark-" (match-string 1))
	    (setq finished t)
	  (add-to-list 'results  (match-string 1)))
	)
      results)
    )
  )


(defun w32-reg-iface-enum-keys (key)
  "Return a list of named keys subkeys.
The KEY specified should be complete, for example
\"HKLM/SOFTWARE/GNU\"
The return key names are not sorted in any way."
  (interactive "sKey")
  (setq key (w32-reg-iface-slash2b key))
  (with-temp-buffer
    (let (
	  (buffer (current-buffer))
	  ;;(buffer (get-buffer-create "TEMP"))
          (default-directory +exec-directory+)
	  (results nil)
	  (finished nil))
      ;;(set-buffer buffer)
      ;;(message "Before")
      (call-process (w32-reg-iface-prog) nil buffer nil
		    "--enum-keys" key)
      ;;(message "%s" (buffer-string))
      (goto-char 0)
      (while (not finished)
	(re-search-forward "^\\(.+\\)$")
	(if (equal ";;CMNT -end-mark-" (match-string 1))
	    (setq finished t)
	  (add-to-list 'results  (match-string 1)))
	)
      results)
    )
  )

(defun w32-reg-iface-culm-enum-keys (subkey)
"Same as `w32-reg-iface-enum-keys', but without root key.
SUBKEY is below root HKCU if exists there, or HKCR otherwise."
  (interactive "sSubkey")
  (setq subkey (w32-reg-iface-slash2b subkey))
  (let ((keys (w32-reg-iface-enum-keys
	       (concat "HKCU\\" subkey))))
    (when (not keys)
      (setq keys (w32-reg-iface-enum-keys
		  (concat "HKLM\\" subkey))))
    keys))



(defun w32-reg-iface-enum-values (key)
  "Return a list of named keys subkeys.
The KEY specified should be complete, for example
\"HKLM/SOFTWARE/GNU/EMACS\"
These value names are not sorted in any way."
  (interactive "sKey")
  (setq key (w32-reg-iface-slash2b key))
  (with-temp-buffer
    (let ((buffer (current-buffer))
          (default-directory +exec-directory+)
	  (results nil)
	  (finished nil))
      (call-process (w32-reg-iface-prog) nil buffer nil
		    "--enum-values" key)
      (goto-char 0)
      (while (not finished)
	(re-search-forward "^\\(.+\\)$")
	(if (equal ";;CMNT -end-mark-" (match-string 1))
	    (setq finished t)
	  (add-to-list 'results  (match-string 1)))
	)
      results) ) )

(defun w32-reg-iface-culm-enum-values (subkey)
  "Same as `w32-reg-iface-enum-values', but without root key.
SUBKEY is below root HKCU if exists there, or HKCR otherwise."
  (interactive "sSubkey")
  (setq subkey (w32-reg-iface-slash2b subkey))
  (let ((values (w32-reg-iface-enum-values
		 (concat "HKCU\\" subkey))))
    (when (not values)
      (setq values (w32-reg-iface-enum-values
		    (concat "HKLM\\" subkey))))
    values))



(defun w32-reg-iface-read-value (key)
  "Return name-value pair for KEY as a cons.
First part of the cons is the value.  Last part of path is the value
type.  If the value name is empty end with just an \\."
  (interactive "sKey")
  (setq key (w32-reg-iface-slash2b key))
  ;;(message "read-value, key=%s" key)
  (with-temp-buffer
    (let ((buffer (current-buffer))
          (default-directory +exec-directory+)
	  (results nil))
      (call-process (w32-reg-iface-prog) nil buffer nil
		    "--read-value" key)
      (goto-char 0)
      (when (re-search-forward "^\\(.+\\)\t\\(.*\\)$" nil t)
	(setq results  (cons (match-string 2) (match-string 1))))
      results
      ) ) )

(defun w32-reg-iface-culm-read-value (subkey)
  "Same as `w32-reg-iface-read-value', but without root key.
SUBKEY is below root HKCU if exists there, or HKCR otherwise."
  (interactive "sSubkey")
  (setq subkey (w32-reg-iface-slash2b subkey))
  (let ((name-value (w32-reg-iface-read-value
		     (concat "HKCU\\" subkey))))
    (when (not name-value)
      (setq name-value (w32-reg-iface-read-value
			(concat "HKLM\\" subkey))))
    name-value))


(defun w32-reg-iface-key-exists (key)
  "Return t if KEY exists, nil otherwise."
  (interactive "sKey")
  (setq key (w32-reg-iface-slash2b key))
  (with-temp-buffer
    (let ((buffer (current-buffer))
          (default-directory +exec-directory+)
	  (exists nil))
      (call-process (w32-reg-iface-prog) nil buffer nil
		    "--read-value" key)
      (goto-char 0)
      (when (re-search-forward "^YES$" nil t)
	(setq exists t))
      exists)))

(defun w32-reg-iface-culm-key-exists (subkey)
  "Same as `w32-reg-iface-key-exists', but without root key.
SUBKEY is below root HKCU if exists there, or HKCR otherwise."
  (interactive "sSubkey")
  (setq subkey (w32-reg-iface-slash2b subkey))
  (let ((exists (w32-reg-iface-key-exists
		 (concat "HKCU\\" subkey))))
    (when (not exists)
      (setq exists (w32-reg-iface-key-exists
		    (concat "HKLM\\" subkey))))
    exists))


;;;;;;;;;;; Obsolete:
(defun w32-reg-iface-query-value ( key path )
  "Interface compatability function.
This function is a simple wrapper that mimics the form
of the read function provided by the registry patch."
  (interactive "sRoot\nsKey")
  (setq key (w32-reg-iface-slash2b key))
  (setq path (w32-reg-iface-slash2b path))
  (w32-reg-iface-read-value (concat key "\\" path)))


;;

;;; Some tests, use "M-x eval-print-last-sexp" to test..
;;
;;(w32-reg-iface-get-root-keys)
;; Should print : ("HKEY_CURRENT_CONFIG" "HKEY_USERS" "HKEY_CLASSES_ROOT" "HKEY_LOCAL_MACHINE" "HKEY_CURRENT_USER")

;; (
;;
;; (w32-reg-iface-enum-keys "HKEY_CURRENT_USER\\Software")

;; Something like : ("Xavier" "wnresqnt" "Windows Crawler" "Wang" "VDO" "VB and VBA Program Settings" "thirty4 interactive" "Tennyson Maxwell" "Systems Internals" "Smd" "SCC" "Q3Radiant" "Policies" "PASSWORD" "ODBC" "Numega" "Nico Mak Computing" "Netscape" "MSJ Bugslayer Column" "mozilla" "mlin" "Microsoft" "Logitech" "Local AppWizard-Generated Applications" "Lighttek" "LHI" "Left Side Software" "lcc" "Kahei" "JASC" "Intel" "InstallShield" "id" "HG Screen Savers" "GNU" "GL Saver" "FrontEnd Plus" "Freeware" "FreeAmp" "FerretSoft" "ES-Computing" "Ensoniq" "ediSys" "DMS Freeware" "Cygnus Solutions" "Classes" "c.igaly" "Aureate" "AndNow East" "AnalogX" "Adobe" "AccuImage Diagnostics Corporation")


;;(w32-reg-iface-enum-keys "HKEY_CURRENT_USER\\Software\\MSJ Bugslayer Column\\CrashFinder")

;;(w32-reg-iface-enum-values "HKEY_CURRENT_CONFIG\\Software\\Microsoft\\Windows\\CurrentVersion\\Internet Settings" )
;;(w32-reg-iface-read-value "HKEY_CURRENT_CONFIG\\Software\\Microsoft\\Windows\\CurrentVersion\\Internet Settings\\ProxyServer")

;;(w32-reg-iface-enum-keys "hkey_local_maching\\software\\gnu")

(provide 'w32-reg-iface)

;;; w32-reg-iface.el ends here
