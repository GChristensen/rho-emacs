;;; menuacc.el --- Add accelerator marks to menu bar

;; Copyright (C) 2005-2008 by Lennart Borgman

;; Author:     Lennart Borgman <lennart DOT borgman AT gmail DOT com>
;; Created: 2005-08-16
;; Version: 0.54
;; Last-Updated: 2008-09-24T11:56:33+0200 Wed
;; Keywords: w32 menus keyboard


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This file adds accelerator to the top level menu names in the menu
;; bar. Those accelerator are put on the first letter of the names so
;; that the same keysequences should be used to access the menus with
;; or without this file.
;;
;; Currently this only works with the patched version of Emacs+EmacsW32.
;;
;; To use this module put it in Emacs load-path and add to your .emacs
;;
;;    (require 'menuacc)
;;
;; and then use custom to set `menuacc-mode'.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; History:
;;


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

(unless (fboundp 'w32-wh-keyboard-ll)
  (error "menuacc currently only works with patched Emacs on w32"))

(eval-when-compile (require 'cl))
(require 'tmm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Debugging etc

(defvar menuacc-make-error nil)

(defvar menuacc-display-error-lwarn nil)
(defvar menuacc-display-error-stop nil)

;;(run-with-idle-timer 1 nil 'menuacc-show-report-message)
(defun menuacc-show-report-message ()
  (let ((msg "menuacc error, please look in the *Message* buffer"))
    (put-text-property 0 (length msg) 'face 'highlight msg)
    (message "%s" msg)))

;;(menuacc-display-error 'test-lwarn-type "testing 1=%s, 2=%s" "one" 'two)
(defun menuacc-display-error (lwarn-type msg &rest args)
  ;; This is borrowed from mumamo.el
  "Display menuacc special errors.
These errors are those happening during menu creation.

A simple error message is shown by 'message. A backtrace colored
with the 'highlight face is placed in the message buffer.

If `menuacc-display-error-lwarn' is non-nil a warning is written
to *Warnings* and this buffer is displayed.

If `menuacc-display-error-stop' is non-nil an error that may stop
fontification is raised."

  ;; Warnings are sometimes disturbning, make it optional:
  (when menuacc-display-error-lwarn
    (apply 'lwarn lwarn-type :error msg args))

  (let ((msg2 (concat "%s: " msg))
        (bt (with-output-to-string (backtrace)))
        (start (+ (with-current-buffer "*Messages*"
                    (point-max))
                  0)))

    ;; Output message together with backtrace:
    (apply 'message msg2 lwarn-type args)

    ;; Does not work here, why?
    ;;(put-text-property 0 (length bt) 'face 'highlight bt)

    ;; Backtrace to message buffer:
    (message "%s" bt)
    (with-current-buffer "*Messages*"
      (goto-char (point-max))
      (backward-char)
      (put-text-property start (point)
                         'face 'highlight))

    ;; Output message once again so the user can see it:
    (apply 'message msg2 lwarn-type args)
    (run-with-idle-timer 1 nil 'menuacc-show-report-message)

    ;; Stop menu creation:
    (when menuacc-display-error-stop
      (apply 'error msg2 lwarn-type args))
    ))

(defun menuacc-debug-to-backtrace (&rest debugger-args)
  (menuacc-display-error 'menuacc-debug-to-backtrace
                         "%s"
                         (nth 1 debugger-args))
  t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Handling accelerator marker:

(defconst menuacc-marker
  (cond
   ((eq window-system 'w32)
    (list ?&))
   ((not window-system)
    nil)
   (t
    (error "System type %s is not yet supported by menuacc.el" system-type))))

(defsubst menuacc-marked (name)
  "Return nil if no accelerator key mark, otherwise nil."
  (let ((tail (memq (car menuacc-marker) (string-to-list name))))
    (when tail
      t                                 ; fix-me
      )))

(defun menuacc-make-accel-text (plain-text accel-char menu)
  (let ((plain-chars (string-to-list plain-text))
        (new-chars nil)
        (added nil))
    (when accel-char (setq accel-char (downcase accel-char)))
    (dolist (c plain-chars)
      (unless (or added
                  (memq c '(?-?. ?\ )))
        (when
            (let ((dc-c (downcase c)))
              (if accel-char
                  (eq dc-c accel-char)
                (not
                 (catch 'used
                   ;; Fix-me: This should perhaps look in the menu-bar:
                   (dolist (used-c menu)
                     (when (eq (string-to-char (nth 1 used-c)) dc-c)
                       (throw 'used t)))
                   ))))
          (progn
            (setq added t)
            (unless accel-char
              (setq accel-char c))
            (dolist (mc menuacc-marker)
              (setq new-chars (cons mc new-chars))))))
      (setq new-chars (cons c new-chars)))
    (unless accel-char (setq accel-char (downcase (car plain-chars))))
    (let ((atext (apply 'string (reverse new-chars))))
      (cons atext accel-char))))

;; (car menuacc-accelerators)
(defvar menuacc-restored nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Predefined marking

(defgroup menuacc nil
  "Customization of menu accelerators."
  :group 'emacsw32
  :group 'convencience)

;; Fix-me: Change the definition of menuacc-accelerators to something
;; better.
;;
;; (customize-option 'temp-char)
;;(defcustom temp-char ?a "doc" :type 'character)
;; Note: Characters are shown ok in custom, but not when one does C-h v.
(defcustom menuacc-accelerators
  '(
    ([menu-bar]
     (
      ;; General
      ("File" "f")
      ("Edit" "e")
      ("Help" "h")
      ;; Common Emacs
      ("Options" "o")
      ("Buffers" "b")
      ("Tools" "t")
      ;; For the rest avoid those above: b e f h o t

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;; Major modes
      ;;  These should not conflict with those above. Only one of them
      ;;  are used at a time. Fix-me: add more.
      ;;
      ;;  Try to use the same accelerator for different major moded to make room for other entries.
      ;;  Use: a c d g i m p s 9
      ("Ada" "a")
      ("Antlr" "a")
      ("AWK" "a")
      ("C" "c")
      ("C++" "c")
      ("Categories" "c")
      ("Class" "c")
      ("CMD" "c")
      ("Connections" "c")
      ("Complete" "c") ;; Not the mode itself
      ("Custom" "c")
      ("Customize" "c")
      ("Debug" "d")
      ("Diff" "d")
      ;;("Ebrowse" "d")
      ("Edebug" "d")
      ("Emacs-Lisp" "a")
      ("Field" "d")
      ("Folder" "d")
      ("F90" "9")
      ("Fortran" "a")
      ("Grep" "p")
      ("Hide" "i")
      ("HTML" "m")
      ("IDL" "d")
      ("IDLWAVE" "a")
      ("In/Out" "i") ;; iu
      ("Info" "i") ;; i
      ("Insert" "i") ;; i
      ("Java" "a")
      ("Lisp-Interaction" "a")
      ("Mail" "m")
      ("Makefile" "m")
      ("Members" "m")
      ("Message" "m")
      ("Meta" "m")
      ("Minibuf" "m")
      ("ObjC" "c")
      ("Octave" "c")
      ("Org" "g")
      ("Outline" "l")
      ("PHP" "p")
      ("Phrases" "p")
      ("Python" "p")
      ("Scheme" "s")
      ("Server" "s")
      ("SES" "s")
      ("SGML" "s")
      ("Signal" "s")
      ("Speedbar" "s")
      ("SQL" "s")
      ("Texinfo" "i") ;; i
      ("VHDL" "d")
      ("Widget" "d")
      ("WoMan" "m")
      ("XML" "m")

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;; Some common minor modes and other important menus
      ;; Try to avoid those letters used above. And try to keep these
      ;; below unique ... ;-)
      ;;
      ("nXhtml" "n")
      ("ECB" "c") ;; clash with some major modes, but that seems to be the best
      ("Headers" "s") ;; In major Mail, m
      ("Hide/Show" "w")
      ("Index" "x") ;; Not the mode itself
      ("Project" "j")
      ("Senator" "r")
      ("Sequence" "s") ;; In major MH
      ))
    ( ;;[menu-bar-file-menu]
     [menu-bar file]
     (
      ("Visit New File..." "n")
      ("Open Recent" "t")
      ("Open File..." "o")
      ("Open Directory..." "d")
      ("Insert File..." "i")
      ("Close" "c")
      ;;
      ("Save" "s")
      ("Save As..." "a")
      ("Save Some Buffers..." "b")
      ("Revert Buffer" "r")
      ("Recover Crashed Session" "h")
      ;;
      ;;
      ("Split Window" "w")
      ("Remove Splits" "v")
      ("New Frame" "f")
      ("New Frame on Display..." "y")
      ("Delete Frame" "l")
      ;;
      ("Quit" "q")
      ))
    ([menu-bar edit]
     (
      ("Undo" "u")
      ("Cut" "t")
      ("Copy" "c")
      ("Paste" "p")
      ("Paste from kill menu" "k")
      ("Clear" "l")
      ("Select All" "a")
      ;;
      ("Search" "s")
      ("Replace" "r")
      ("Go To" "g")
      ("Bookmarks" "b")
      ;;
      ("Text Properties" "t")
      ("Regexp Highlighting" "x")
      ))
    ([menu-bar options]
     (
      ("Active Region Highlighting" "h")
      ("Paren Match Highlighting" "p")
      ;;
      ("Truncate Long Lines in this Buffer" "l")
      ("Word Wrap in Text Modes" "w")
      ("Case-Insensitive Search" "i")
      ("C-x/C-c/C-v Cut and Paste (CUA)" "c")
      ;;
      ("Use Directory Names in Buffer Names" "d")
      ("Save Place in Files between Sessions" "f")
      ;;
      ("Blinking Cursor" "b")
      ("More Noticeable Prompt" "n")
      ;;
      ("Enter Debugger on Error" "e")
      ("Enter Debugger on Quit/C-g" "q")
      ;;
      ("Mule (Multilingual Environment)" "m")
      ;;
      ("Show/Hide" "w")
      ("Set Font/Fontset..." "o")
      ;;
      ("Save Options" "s")
      ("Customize Emacs" "z")
      ))
    ([menu-bar buffer]
     (
      ("Frames" "f")
      ;;
      ("Next Buffer" "n")
      ("Previous Buffer" "p")
      ("Select Named Buffer..." "s")
      ("List All Buffers" "l")
      ))
    ([menu-bar tools]
     (
      ("Search Files (Grep)..." "s")
      ("Compile..." "i")
      ("Shell Command..." "c")
      ("Shell Command on Region..." "r")
      ("Debugger (GDB)..." "d")
      ;;
      ("Spell Checking" "k")
      ;;
      ("Compare (Ediff)" "e")
      ("Merge" "m")
      ("Apply Patch" "p")
      ("Ediff Miscellenea" "m")
      ;;
      ("Version Control" "v")
      ;;("PCL-CVS" "?")
      ;;
      ("Read Net News (Gnus)" "g")
      ;;; Fix me: The two items below are generated each time, see
      ;;; menu-bar.el, line 1296
      ;;("Read Mail (with RMAIL)" "a")
      ;;("Send Mail (with sendmail)" "l")
      ("Directory Search" "y")
      ;;
      ("Calendar" "n")
      ("Programmable Calculator" "o")
      ("Simple Calculator" "u")
      ;;
      ("Games" "g")
      ))
    ([menu-bar help-menu]
     (
      ("Emacs Tutorial" "t")
      ("Emacs FAQ" "f")
      ("Emacs News" "n")
      ("Emacs Known Problems" "k")
      ("Send Bug Report..." "b")
      ("Emacs Psychotherapist" "y")
      ;;
      ("Search Documentation" "s")
      ("Describe" "d")
      ("Read the Emacs Manual" "m")
      ("More Manuals" "o")
      ("Find Emacs Packages" "p")
      ("External Packages" "x")
      ;;
      ("Getting New Versions" "v")
      ("Copying Conditions" "c")
      ("(Non)Warranty" "w")
      ;;
      ("About Emacs" "a")
      ("About GNU" "g")
      ))
    )
  "Default menu accelerators in the menu bar."
  :type
  '(repeat
    (list
     sexp
     (repeat
      (list string string)
      )
     )
    )
  :group 'menuacc)
;; (customize-option 'menuacc-accelerators)
;; (set 'menuacc-accelerators nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Adding the marking

(defun menuacc-tmm-get-keybind (keyseq)
  "Return the current binding of KEYSEQ, merging prefix definitions.
If KEYSEQ is a prefix key that has local and global bindings,
we merge them into a single keymap which shows the proper order of the menu.
However, for the menu bar itself, the value does not take account
of `menu-bar-final-items'."
  (let (allbind bind minorbind localbind globalbind)
    (setq bind (key-binding keyseq))
    ;; If KEYSEQ is a prefix key, then BIND is either nil
    ;; or a symbol defined as a keymap (which satisfies keymapp).
    (if (keymapp bind)
	(setq bind nil))
    ;; If we have a non-keymap definition, return that.
    (or bind
	(progn
	  ;; Otherwise, it is a prefix, so make a list of the subcommands.
	  ;; Make a list of all the bindings in all the keymaps.
	  (setq minorbind (mapcar 'cdr (minor-mode-key-binding keyseq)))
	  (setq localbind (local-key-binding keyseq))
          ;; This function is the same as tmm-get-keybind except for
          ;; the next line.
          (if (eq 1 localbind) (setq localbind nil))
	  (setq globalbind (copy-sequence (cdr (global-key-binding keyseq))))

	  ;; If items have been redefined/undefined locally, remove them from
	  ;; the global list.
	  (dolist (minor minorbind)
	    (dolist (item (cdr minor))
	      (setq globalbind (assq-delete-all (car-safe item) globalbind))))
	  (dolist (item (cdr localbind))
	    (setq globalbind (assq-delete-all (car-safe item) globalbind)))

	  (setq globalbind (cons 'keymap globalbind))
	  (setq allbind (cons globalbind (cons localbind minorbind)))

	  ;; Merge all the elements of ALLBIND into one keymap.
	  (mapc (lambda (in)
		  (if (and (symbolp in) (keymapp in))
		      (setq in (symbol-function in)))
		  (and in (keymapp in)
		       (if (keymapp bind)
			   (setq bind (nconc bind (copy-sequence (cdr in))))
			 (setq bind (copy-sequence in)))))
                allbind)
	  ;; Return that keymap.
	  (list bind globalbind)))))

(defvar menuacc-temp-trace nil)

(defun menuacc-add-accel-1 (menu)
  (when menuacc-make-error (error "menuacc-add-accel: making an error"))
  (progn
    (let ((menubs (menuacc-tmm-get-keybind menu))
          (menuacc-temp-changed nil)
          (current-accel (cadr (assoc menu menuacc-accelerators)))
          names
          tmm-km-list
          tmm-table-undef
          (trace nil)) ;(equal menu [menu-bar edit])))
      ;; The implementation below is just a try based on how things
      ;; are done in tmm.  Hopefully the code below is sufficient
      ;; correct for our purpose here.
      ;;
      ;; In first pass use the accelerators defined in
      ;; current-accel. In second pass add accelerators to the
      ;; rest.
      (when trace (message "menuacc,current-accel=%S" current-accel))
      (dolist (menub (when (listp menubs) menubs)) ;; menubs can be 'undefined
        (dolist (pass '(first second))
          (mapc (lambda (km)
                  (setq trace nil)
                  (let ((tail (when (listp km) (cdr km)))
                        name
                        (new-name ""))
                    (when (and tail
                               (listp tail))
                      (setq name (car tail))
                      (unless (stringp name)
                        (when (memq name '(menu-item cons))
                          (setq tail (cdr tail))
                          (setq name (car tail))))
                      (if (stringp name)
                          (progn
                            (when (and (> (length name) 3)
                                       (string= "Frx" (substring name 0 3))
                                       (not menuacc-temp-trace))
                              (setq menuacc-temp-trace t)
                              (setq trace t))
                            (when trace (message "menuacc,backtrace:\n%S" (with-output-to-string (backtrace))))
                            (when trace (message "menuacc,pass=%s" pass))
                            (unless (or (string= "--" name)
                                        (menuacc-marked name))
                              (let* ((old (assoc name current-accel))
                                     name-accel)
                                (when trace (message "name=%s, old=%s" name old))
                                (if old
                                    (setq name-accel
                                          (menuacc-make-accel-text (nth 0 old)
                                                                   (string-to-char (nth 1 old))
                                                                   current-accel))
                                  (when (eq pass 'second)
                                    (setq menuacc-temp-changed t)
                                    (setq name-accel (menuacc-make-accel-text name nil current-accel))
                                    (add-to-list 'current-accel (list name (downcase (char-to-string (cdr name-accel)))))))
                                (setq new-name (car name-accel)))
                              (when new-name (setcar tail new-name))))
                        (unless (memq (car km) '(buffer rmail compose-mail))
                          ;;(message "menuacc-add-accel: Can't add accel, km=%S, name=%S, menub=%S" km name menub)
                          (message "menuacc-add-accel: Can't add accel, km=%S, name=%S" km name)
                          (message ""))
                        ))))
                menub))))))

;; (defun emacs-Q-menuacc()
;;   "Start new Emacs with -Q and load menuacc (+ w32-meta)."
;;   (interactive)
;;   (let* ((this-file (locate-library "menuacc"))
;;          (this-dir (file-name-directory this-file))
;;          (meta-file (locate-library "w32-meta"))
;;          )
;;     (call-process (ourcomments-find-emacs) nil 0 nil "-Q"
;;                   "--debug-init"
;;                   "--load" meta-file
;;                   "-f" "w32-meta-lr"
;;                   "--load" this-file
;;                   "-f" "menuacc-mode")
;;     (message "Started 'emacs -Q --load \"%s\"' - it will be ready soon ..."
;;              this-file)))

(defun menuacc-add-accel (from-menu-bar-update-hook)
  (menuacc-add-accel-1 [menu-bar])
  (if from-menu-bar-update-hook
      ;; Delay the big job, gives some small problems, but ...
      (add-hook 'post-command-hook 'menuacc-add-accel-from-post-command-hook)
    (menuacc-add-accel-1 [menu-bar file])
    (menuacc-add-accel-1 [menu-bar edit])
    (menuacc-add-accel-1 [menu-bar options])
    (menuacc-add-accel-1 [menu-bar buffer])
    (menuacc-add-accel-1 [menu-bar tools])
    (menuacc-add-accel-1 [menu-bar help-menu])))

(defun menuacc-add-accel-from-menu-bar-update-hook ()
  (let ((debugger 'menuacc-debug-to-backtrace)
        (debug-on-error t))
    (menuacc-add-accel t)))

(defun menuacc-add-accel-from-post-command-hook ()
  (condition-case err
      (let ((debugger 'menuacc-debug-to-backtrace)
            (debug-on-error t))
        (remove-hook 'post-command-hook 'menuacc-add-accel-from-post-command-hook)
        (menuacc-add-accel nil))
    (error (message "menuacc-add-accel:%s"
                    (error-message-string err)))))

(define-minor-mode menuacc-mode
  "Add underlined accelerators to menus.
This is done for the menu bar, the File, Edit, Option, Buffers,
Tools and Help menu. The menu bar is updated dynamically."
  :init-value nil
  :global t
  :group 'menuacc
  ;;:require 'menuacc
  (if (and menuacc-mode
           window-system)
      (when window-system
        (run-hooks 'menu-bar-update-hook)
        (menuacc-add-accel nil)
        (add-hook 'menu-bar-update-hook 'menuacc-add-accel-from-menu-bar-update-hook t)
        )
    (remove-hook 'post-command-hook 'menuacc-add-accel-from-post-command-hook)
    (remove-hook 'menu-bar-update-hook 'menuacc-add-accel-from-menu-bar-update-hook)
    ))

(provide 'menuacc)

;;; menuacc.el ends here
