;;; w32-regdat.el --- Fetch important values from the Registry
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2005-08-03
;; Version: 0.87
;; Last-Updated: 2008-03-06T01:22:47+0100 Thu


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; For internal use in emacsw32 mostly.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; History:
;;
;; 2005-11-22: Corrected several errors in "Find All!".
;;
;; FIX-ME: when using "find all" errors are not collected.


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

(require 'w32-reg-iface)
(require 'setup-helper)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GIMP
;;

(defun w32-regdat-gimp-win-remote-cmd ()
  "Return GIMP remote command string."
  (let ((cmd (w32-reg-iface-culm-read-value
                 "SOFTWARE\\Classes\\TheGIMP20\\shell\\Open with GIMP\\command\\"
                 )))
    (when cmd
      (car cmd))))
;;(w32-regdat-gimp-win-remote-cmd)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ImageMagick
;;

(defun w32-regdat-imagemagick-bindir ()
  "Return ImageMagic bin directory."
  (let ((bindir (w32-reg-iface-culm-read-value "SOFTWARE\\ImageMagick\\Current\\BinPath")))
    (when bindir
      (car bindir))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cygwin
;;

(defun w32-regdat-cygwin-bindir ()
  "Return Cygwin bin directory."
  (let ((bindir (w32-reg-iface-culm-read-value "SOFTWARE\\Cygnus Solutions\\Cygwin\\mounts v2\\/\\native")))
    (when bindir
      (concat (car bindir) "\\bin"))))

;; (w32-regdat-cygwin-bindir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MSYS
;;
(defun w32-regdat-msys-bindir ()
  "Get MSYS path from uninstall info in Windows Registry."
  (let* ((uninsroot "HKLM/SOFTWARE/Microsoft/Windows/CurrentVersion/Uninstall/")
         (uninskeys (w32-reg-iface-enum-keys uninsroot))
         msyskey
         valkeypath
         msyspath
         )
    (mapc (lambda (key)
            ;;(when (string-match "^MSYS" key) (message "is msys=%s" key)(sit-for 2))
            (when (string-match "^MSYS-[[:digit:]]" key)
              ;;(message "key=%s" key)(sit-for 2)
              (setq msyskey key)))
          uninskeys)
    (if msyskey
        (progn
          (setq valkeypath (concat uninsroot msyskey "\\Inno Setup: App Path"))
          ;;(message "valpath=%s" valkeypath)(sit-for 5)
          (setq msyspath (concat (car (w32-reg-iface-read-value valkeypath)) "\\bin")))
      (message "Can't find MSYS, is it installed?")(sit-for 5))
    msyspath
    ))
;; (w32-regdat-msys-bindir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keyboard layout
;;
;; hklm\\software\\microsoft\\windows\\currentversion\\nls\\LocalMapIDs
;; hklm\\system\\currentcontrolset\\control\\keyboard layout\\DosKeybCodes
;; hklm\\system\\currentcontrolset\\control\\keyboard layouts\\0000041D


(defun w32-regdat-kb-layout-preload ()
  "Return a list (short-name long-name id)."
  (let ((val-type-code
         (w32-reg-iface-culm-read-value "Keyboard Layout\\Preload\\1")))
    (if val-type-code
	(let* (
	       (kbcode (car val-type-code))
	       (val-type-short
		(w32-reg-iface-read-value
		 (concat
		  "hklm\\system\\currentcontrolset\\control\\keyboard layout\\DosKeybCodes\\"
		  kbcode)))
	       ;; Assoc list of ms incorrect two letter language spec and corr ISO spec
	       (ms-to-iso (list
			   (cons "sv" "se")
			   ))
	       (two-letter (let* ((two (car val-type-short))
				  (atwo (assoc two ms-to-iso)))
			     (if atwo (cdr atwo) two)))
	       (val-type-long
		(w32-reg-iface-read-value
		 (concat
		  "hklm\\system\\currentcontrolset\\control\\keyboard layouts\\"
		  kbcode "\\layout text")))
	       (long-name (car val-type-long))
	       )
	  (list two-letter long-name kbcode))
      nil)))

;;(message "%s" (w32-regdat-kb-layout-preload))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 7-zip

(defun w32-regdat-7z-dir ()
"Returns 7z directory where you find the executables.
The command line programs a 7za.exe and 7z.exe."
  (let ((val-type (w32-reg-iface-culm-read-value "software\\7-zip\\path")))
    (if val-type
	(car val-type)
      nil) ))

;;(message "%s" (w32-regdat-7z-dir))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Paper size (not a program, but used by some...)

(defvar w32-regdat-winforms-prog
  (convert-standard-filename
   (expand-file-name
    (concat +exec-directory+ "../../EmacsW32/bin/winforms.exe")
    "Winforms.exe full path")))

(defun w32-regdat-get-paper-size ()
  "Get default paper size. Not finished yet!"
  (interactive)
  (with-temp-buffer
    (let (
	  (buffer (current-buffer))
	  (paper nil)
          (default-directory +exec-directory+)
	  (finished nil))
      ;;(message "Before")
      (call-process w32-regdat-winforms-prog nil buffer nil "--paper")
      (message "%s" (buffer-string)) ;;(sit-for 2)
      ;;(erase-buffer)
      ;;(message "after erase=(%s)" (buffer-string)) (sit-for 2)
      ;;(goto-char 0)
      ;;(princ "Paper Type: (YY)\n" (current-buffer))
      ;;(message "after princ=(%s)" (buffer-string)) (sit-for 2)
      (goto-char 0)
      (while (not finished)
	(if (not (re-search-forward "^\\([^:]+\\): (\\(.+\\))$" nil t))
	    (progn
	      (message "%s" "did not match") (sit-for 2)
	      (setq finished t))
	  ;;(message "ms1=%s" (match-string 1)) (sit-for 2)
	  ;;(message "ms2=%s" (match-string 2)) (sit-for 2)
	  (when (equal "Default printer" (match-string 1))
	    (when (equal "not found" (match-string 2))
	      (message "No default printer") (sit-for 5)
	      (setq finished t)))
	  (when (equal "Default paper type" (match-string 1))
	    (setq paper (match-string 2))
	    (message "found paper=%s" paper) ;;(sit-for 2)
	    (setq finished t))))
      (message "return paper=%s" paper) ;;(sit-for 2)
      paper)))
;;   "Not finished yet - I don't know the possible values...
;; And I do not know which is the default value!"
;;   (let ((paplst (mapcar 'upcase (w32-reg-iface-enum-values
;; 				 "HKLM\\System\\CurrentControlSet\\Control\\Print\\Forms")))
;; 	)
;;     (if (or (memq "A4" paplst)
;; 	    (memq "8.5x13" paplst)) ;; More alternatives??
;; 	"A4"
;;       (if (memq "LETTER" paplst) ;; More alternatives??
;; 	  "LETTER"
;; 	;; Return nil if nothing was recognized
;; 	nil))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This is the very cumbersome search for Ghostscript!
;; The code is modelled after code in GSview.

(defvar w32-regdat-gs-family
  (list "AFPL Ghostscript" "Aladdin Ghostscript" "GPL Ghostscript" "GNU Ghostscript")
  "List of known Ghostscript products.
Used in registry names.  From GSview sources.")

(defun w32-regdat-get-gs-ver-list (family)
"Get a list of installed versions for this Ghostscript FAMILY."
  (w32-reg-iface-culm-enum-keys
   (concat "Software\\" family)))

(defun w32-regdat-get-gs-max-verf (family)
"Get max installed version for the Ghostscript FAMILY."
  (let ((versions (w32-regdat-get-gs-ver-list family)))
    (when versions
      (apply 'max (mapcar 'string-to-number versions)))))

(defun w32-regdat-get-gs-max-ver ()
"Get max installed version for the Ghostscript family."
  (let ((maxes (delete nil (mapcar 'w32-regdat-get-gs-max-verf w32-regdat-gs-family))))
    (when maxes
      (apply 'max maxes))))

(defun w32-regdat-get-gs-stringf (family version valname)
"For GhostScript FAMILY VERSION get VALNAME from Registry."
  (let ((key (concat "Software\\" family "\\"
		     (number-to-string version)
		     "\\" valname
		     )))
    (w32-reg-iface-culm-read-value key)))

(defun w32-regdat-get-gs-string (version val-name)
  "For \(max\) VERSION of Ghostscript get VAL-NAME from Registry."
  (message "w32-regdat-get-gs-string")
  (when version
    (let ((gs-string)
	  (gs-family))
      (mapc (lambda (elem)
	      (let ((this-string (w32-regdat-get-gs-stringf elem version val-name)))
		(when this-string
		  (setq gs-family elem)
		  (setq gs-string this-string))))
	    w32-regdat-gs-family)
      (if gs-string
	  (cons gs-family gs-string)
	nil))))

(defun w32-regdat-find-gs-dll-or-exe (b-dll)
"Find dll or exe path."
  (let* (
	 (gs-ver (w32-regdat-get-gs-max-ver))
	 (gs-family-path (w32-regdat-get-gs-string gs-ver "GS_DLL"))
	 (gs-family (car gs-family-path))
	 (gs-dll (car (cdr gs-family-path)))
	 (gs-path (when gs-dll
		    (if b-dll gs-dll
		      (concat (file-name-directory gs-dll) "gswin32c.exe")))))
    ;;(list gs-ver gs-family-path gs-family gs-dll gs-path)
    gs-path
    ))

(defun w32-regdat-find-gs ()
"Find GhostScript .exe-file."
  (let ((gs-path (w32-regdat-find-gs-dll-or-exe nil)))
    (when gs-path (setq gs-path (convert-standard-filename gs-path)))
    gs-path))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Much easier to find GhostView!

(defun w32-regdat-find-gv ()
"Find GSview .exe-file."
  (let* ((val-name
	  (w32-reg-iface-read-value
	   "HKLM\\SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\App Paths\\gsview32.exe\\"))
	 (gv-path (car val-name)))
    (when gv-path
      (setq gv-path (convert-standard-filename gv-path))
      (if (file-readable-p gv-path) gv-path nil))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Saving


(defun w32-regdat-addcust-all ()
  "Search for  values and add them to Emacs customize.
Only adds the corresponding variables if it is boundp and have its
default value.  Does not save values. Call (custom-save-all) to save.

Does not load any libraries."
  (require 'setup-helper)
  ;;(require 'tumme)
  (require 'image-dired)
  ;;(require 'printing nil t)
  ;;(when (featurep 'printing)

  (when (featurep 'thumbs)
    (let ((bindir (w32-regdat-imagemagick-bindir)))
      (when bindir
        (let ((default (eval (car (get 'image-dired-cmd-create-thumbnail-program 'standard-value)))))
          ;;(when (equal image-dired-cmd-create-thumbnail-program default)
          (unless (or (file-exists-p default)
                      ;; Program name is convert.exe which clashes
                      ;; with w32 convert.exe
                      ;;(executable-find default)
                      )
            (let ((exe (expand-file-name "convert.exe" bindir)))
              (when (file-executable-p exe)
                (setup-helper-custom-set-to-save 'image-dired-cmd-create-thumbnail-program exe)
                ))))
        (let ((default (eval (car (get 'image-dired-cmd-create-temp-image-program 'standard-value)))))
          ;;(when (equal image-dired-cmd-create-temp-image-program default)
          (unless (or (file-exists-p default)
                      ;; Program name is convert.exe which clashes
                      ;; with w32 convert.exe
                      ;;(executable-find default)
                      )
            (let ((exe (expand-file-name "convert.exe" bindir)))
              (when (file-executable-p exe)
                (setup-helper-custom-set-to-save 'image-dired-cmd-create-temp-image-program exe)
                ))))
        (let ((default (eval (car (get 'image-dired-cmd-rotate-thumbnail-program 'standard-value)))))
          ;;(when (equal image-dired-cmd-rotate-thumbnail-program default)
          (unless (or (file-exists-p default)
                      ;; Program name is convert.exe which clashes
                      ;; with w32 convert.exe
                      ;;(executable-find default)
                      )
            (let ((exe (expand-file-name "mogrify.exe" bindir)))
              (when (file-executable-p exe)
                (setup-helper-custom-set-to-save 'image-dired-cmd-rotate-thumbnail-program exe)
                ))))
        )))

  (when (boundp 'pr-gv-command)
    (let ((default (eval (car (get 'pr-gv-command 'standard-value)))))
      ;;(when (equal pr-gv-command default)
      (unless (or (file-exists-p default)
                  (executable-find default))
        (let ((gv (w32-regdat-find-gv)))
          (when gv
            (setup-helper-custom-set-to-save 'pr-gv-command gv)
            )))))

;  (when (boundp 'pr-gs-command)
;    (let ((default (eval (car (get 'pr-gs-command 'standard-value)))))
;      ;;(when (equal pr-gs-command default)
;      (unless (or (file-exists-p default)
;                  (executable-find default))
;        (let ((gs (w32-regdat-find-gs)))
;          (when gs
;            (setup-helper-custom-set-to-save 'pr-gs-command gs)
;            )))))

  (when (boundp 'ps-paper-type)
    (let ((default (eval (car (get 'ps-paper-type 'standard-value)))))
      ;;(when (equal ps-paper-type default)
      (let ((paper (w32-regdat-get-paper-size)))
        (when (and paper
                   (not (equal paper default)))
          (setup-helper-custom-set-to-save 'ps-paper-type paper)
          ))))

  (when (boundp 'w32shell-cygwin-bin)
    (let ((default (eval (car (get 'w32shell-cygwin-bin 'standard-value)))))
      ;;(message "default=%s" default)(sit-for 2)
      ;;(when (equal w32shell-cygwin-bin default)
      (let ((cygwin (w32-regdat-cygwin-bindir)))
        ;;(message "cygwin=%s" cygwin)(sit-for 2)
        (when (and cygwin
                   (not (equal cygwin default)))
          (setup-helper-custom-set-to-save 'w32shell-cygwin-bin cygwin)
          ))))

  (when (boundp 'w32shell-msys-bin)
    (let ((default (eval (car (get 'w32shell-msys-bin 'standard-value)))))
      ;;(message "default=%s" default)(sit-for 2)
      ;;(when (equal w32shell-msys-bin default)
      (let ((msys (w32-regdat-msys-bindir)))
        ;;(message "msys=%s" msys)(sit-for 2)
        (when (and msys
                   (not (equal msys default)))
          (setup-helper-custom-set-to-save 'w32shell-msys-bin msys)
          ))))

;;  (setup-helper-add-custom 'w32shell-cygwin-bin nil "hej")
;;   (when (boundp 'altgr-language-keyboard)
;;     (let ((default (eval (car (get 'altgr-language-keyboard 'standard-value)))))
;;       (when (equal altgr-language-keyboard default)
;; 	(let ((kb-list (w32-regdat-kb-layout-preload)))
;; 	  ;;"Returns a list (short-name long-name id)."
;; 	  (when kb-list
;; 	    (let ((two-letter (car kb-list)))
;; 	      (unless (equal two-letter default)
;; 		(setup-helper-add-custom 'altgr-language-keyboard nil two-letter) )))))))
  )

;;(w32-regdat-addcust-all)
;;(custom-save-all)

;;(message "%s" (w32-regdat-find-gv))
;;(message "%s" (w32-regdat-find-gs))
;;(w32-regdat-get-gs-max-verf "afpl ghostscript")
;;(w32-regdat-get-gs-max-ver)
;;(w32-regdat-get-gs-string 8.14 "GS_DLL")
;;
;;(w32-regdat-get-gs-stringf "afpl ghostscript" 8.14 "GS_DLL")

(provide 'w32-regdat)

;;; w32-regdat.el ends here
