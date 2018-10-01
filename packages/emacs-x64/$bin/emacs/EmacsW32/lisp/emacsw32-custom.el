;;; emacsw32-custom.el --- Easy customization for emacsw32
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2005-09-22
;; Last-Updated: 2008-01-20T02:03:01+0100 Sun
;; Version: 0.53
;; Keywords: installation setup



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This file is not part of Emacs

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; To find out more about the GNU General Public License you can visit
;; Free Software Foundation's website http://www.fsf.org/.  Or, write
;; to the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:

;; This is a collection of functions maybe useful when installing or
;; setting up emacs.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; History:
;;
;; 2005-12-29: Values could include faces. Corrected.
;;
;; 2006-02-01: Customizations were not alwasy saved. Fixed.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:
(eval-and-compile (require 'wid-edit))

(eval-when-compile (require 'cl))
(require 'cus-edit)
(require 'printing)
(require 'emacsw32)
(require 'w32-regdat)
(require 'noprint)
(require 'w32shell)
(require 'image-dired)
(when (fboundp 'w32-set-wh-keyboard-ll) (require 'w32-meta))



(defun emacsw32-custom-valfaced (value &optional bgcolor)
  (propertize value 'face `(:weight bold :background ,(or bgcolor "RGB:FF/FF/BB"))))

(defun emacsw32-custom-set (symbol value)
  ;;(setup-helper-add-custom symbol value)
  ;;(setup-helper-set-as-custom symbol)
  (setup-helper-custom-set-to-save symbol value))


(defun emacsw32-custom-save-all ()
  "Like `custom-save-all', but save only if custom file is ok."
  (let ((ok-to-save (condition-case err
			(progn
			  (custom-file)
			  (setq ok-to-save t))
		      (error nil))))
     (when ok-to-save 
       (custom-save-all))))


;; FIX-ME: It should not be here - and perhaps not at all!
(defface emacsw32-link-face
  `((((class color) (background light)) (:foreground "blue" :underline t))
    (((class color) (background dark)) (:foreground "cyan" :underline t)))
  "Face used to highlight attributes that are links."
  :group 'emacsw32)


(defun emacsw32-custom-url-link (txt url)
  (let ((plain-url (substring-no-properties url)))
    (unless (equal txt url)
      (put-text-property 0 (length txt) 'help-echo plain-url txt))
    (put-text-property 0 (length txt) 'mouse-face 'highlight txt)
    ;;(put-text-property 0 (length txt) 'keymap w32-feelin-url-keymap txt)
    (let ((btn (widget-create
                'push-button
                :notify 'emacsw32-notify-btn-url
                :button-face 'emacsw32-link-face
                :mouse-face 'highlight
                :button-prefix ""
                :button-suffix ""
                txt)))
      (widget-put btn 'url url))))

(defun emacsw32-custom-describe-symbol (txt symbol)
  (let ((plain-url (symbol-name symbol)))
    (unless (equal txt symbol)
      (put-text-property 0 (length txt) 'help-echo plain-url txt))
    (put-text-property 0 (length txt) 'mouse-face 'highlight txt)
    ;;(put-text-property 0 (length txt) 'keymap w32-feelin-url-keymap txt)
    (let ((btn (widget-create
                'push-button
                :notify (lambda (w &rest ign)
                          (let ((sym (widget-get w 'url)))
                            (if (fboundp sym)
                                (describe-function sym)
                              (describe-variable sym))))
                :button-face 'emacsw32-link-face
                :mouse-face 'highlight
                :button-prefix ""
                :button-suffix ""
                txt)))
      (widget-put btn 'url symbol))))

(defun emacsw32-notify-btn-url (widget &rest ignore)
  (browse-url (widget-get widget 'url)))


(defun emacsw32-custom-divider (length)
  (let ((s (concat (make-string length ?_) "\n")))
    (put-text-property 0 (length s)
                       'face '(:weight bold
                                       :height 1.5
                                       :foreground "maroon") s)
    s))

(defun emacsw32-custom-h1(title &optional divider top-newline)
  (let ((s title))
    (put-text-property 0 (length s)
                       'face (list :inherit 'variable-pitch
                                   :weight 'bold
                                   :height 1.5
                                   :foreground "maroon"
                                   ;;:underline t
                                   )
                       s)
    (when top-newline (widget-insert "\n"))
    ;;(when divider (widget-insert (emacsw32-custom-divider (length s))))
    (when divider (widget-insert "\n"))
    (widget-insert s)
    ))

(defun emacsw32-nice-symbol-value (sym)
  (let ((val (symbol-value sym)))
    (cond
     ((eq val t) "on")
     ((eq val nil) "off")
     ((eq val 'emacs) "use Alt key as Meta")
     ((eq val 'w32-lr) "left+right win keys as Meta")
     ((eq val 'w32-l)  "left win key as Meta")
     ((eq val 'w32-r)  "right win key as Meta")
     ((eq val 'w32)    "neither win keys nor Alt key as Meta")
     (t val))))

(defun emacsw32-custom-insert-emacsw32-row (symbol emacsw32-value &optional description)
  (let ((desc (if description
                   (format "%s" description)
                 (format "%s" (custom-unlispify-tag-name symbol))))
        (link-text (format "%s" symbol)))
    (widget-insert "  " desc " (")
    (emacsw32-custom-describe-symbol link-text symbol)
    (widget-insert "): "
                   (emacsw32-custom-valfaced
                    (format "%s" (emacsw32-nice-symbol-value symbol))
                    (if (eq (symbol-value symbol)
                            emacsw32-value)
                        "GreenYellow"
                      "gainsboro"))
                   "\n")))


(defun emacsw32-cygwin-find-notify (&rest ignore)
  (message "Searching for Cygwin...")
  (let ((cygwin (w32-regdat-cygwin-bindir)))
    (when cygwin
      (emacsw32-custom-set 'w32shell-cygwin-bin cygwin)
      (emacsw32-custom-save-all))
    (emacsw32-custom-start "w32shell-cygwin-bin"
                           (unless cygwin "Not found"))))

(defun emacsw32-find-spell-program ()
  (let ((spell-prog
         (or (executable-find "aspell")
             (when w32shell-cygwin-bin
               (let ((prog (expand-file-name
                            "aspell.exe"
                            (file-name-as-directory w32shell-cygwin-bin))))
                 (when (file-exists-p prog) prog)))
             (executable-find "ispell"))))
    (when spell-prog
      (emacsw32-custom-set 'ispell-program-name spell-prog)
      (emacsw32-custom-save-all))
    spell-prog))

(defun emacsw32-custom-start (&optional updated error)
  "Show page Customize EmacsW32."
  (interactive)
  (require 'appmenu)
  (require 'hfyview)
  (require 'ispell)
  (require 'ourcomments-util)
  (require 'sex-mode)
  (require 'tabkey2)
  ;;(setq debug-on-error t)
  (switch-to-buffer "*Customize EmacsW32*")
  (kill-all-local-variables)
  (if (fboundp 'use-custom-style)
      (use-custom-style)
    (Custom-mode))
  (let ((inhibit-read-only t))
    (erase-buffer))
  (let ((sFound "found")
        (sError "error"))
    (put-text-property 0 (length sFound)
                       'face '(bold
                               (foreground-color . "green")) sFound)
    (put-text-property 0 (length sError)
                       'face '(bold
                               (foreground-color . "red")) sError)
    (let* (
           (default-used "(not set yet - default used)")
           (gs-ok nil)
           (gs-msg (concat
                    "Current GhostScript program: "
                    (if (setup-helper-customizeable 'pr-gs-command)
                        (if pr-gs-command
                            (if (file-exists-p pr-gs-command)
                                (progn
                                  (setq gs-ok t)
                                  (emacsw32-custom-valfaced pr-gs-command))
                              (concat "(" sError ": can't find " pr-gs-command ")"))
                          default-used)
                      "(Need printing.el for this)")
                    "\n"))
           (gv-ok nil)
           (gv-msg (concat
                    "Current GSview program: "
                    (if (setup-helper-customizeable 'pr-gv-command)
                        (if pr-gv-command
                            (if (file-exists-p pr-gv-command)
                                (progn
                                  (setq gv-ok t)
                                  (emacsw32-custom-valfaced pr-gv-command))
                              (concat "(" sError ": can't find " pr-gv-command ")"))
                          default-used)
                      "(Need printing.el for this)")
                    "\n"))
           (paper-ok nil)
           (paper-msg (concat
                       "Current paper type: "
                       (if ps-paper-type
                           (emacsw32-custom-valfaced (format "%s" ps-paper-type))
                         default-used)
                       "\n"))
           (cygwin-msg (concat
                      "Current Cygwin path: "
                      (if (setup-helper-customizeable 'w32shell-cygwin-bin)
                          (if (equal w32shell-cygwin-bin "")
                              (emacsw32-custom-valfaced "(none)")
                            (if (file-exists-p (expand-file-name "sh.exe" w32shell-cygwin-bin))
                                (emacsw32-custom-valfaced w32shell-cygwin-bin)
                              (concat
                               "(" sError
                               (if (file-directory-p w32shell-cygwin-bin)
                                   ": Not an Cygwin bin dir "
                                 ": Can't find directory ")
                               w32shell-cygwin-bin
                               ")"
                               )))
                        "(Need w32shell.el for this)")
                      "\n"))
           (msys-msg (concat
                      "Current MSYS path: "
                      (if (setup-helper-customizeable 'w32shell-msys-bin)
                          (if (equal w32shell-msys-bin "")
                              (emacsw32-custom-valfaced "(none)")
                            (if (file-exists-p (expand-file-name "sh.exe" w32shell-msys-bin))
                                (emacsw32-custom-valfaced w32shell-msys-bin)
                              (concat
                               "(" sError
                               (if (file-directory-p w32shell-msys-bin)
                                   ": Not an MSYS bin dir "
                                 ": Can't find directory ")
                               w32shell-msys-bin
                               ")"
                               )))
                        "(Need w32shell.el for this)")
                      "\n"))
           (spell-msg (concat
                       "Current spell program path: "
                       (if (executable-find ispell-program-name)
                           (emacsw32-custom-valfaced ispell-program-name)
                         (concat " (" sError ": Can't find "
                                 ispell-program-name
                                 ")"))
                       "\n"))
           (image-dired-msg (concat
                       "Current create thumbnail program: "
                       (if (setup-helper-customizeable 'image-dired-cmd-create-thumbnail-program)
                           (if (equal image-dired-cmd-create-thumbnail-program "")
                               (emacsw32-custom-valfaced "(none)")
                             (let* ((full-file
                                     (if (file-exists-p image-dired-cmd-create-thumbnail-program)
                                         image-dired-cmd-create-thumbnail-program
                                       (executable-find image-dired-cmd-create-thumbnail-program)))
                                    (windir (file-name-directory (executable-find "notepad.exe")))
                                    (inwindir (and full-file
                                                   (string= windir
                                                            (file-name-directory full-file))))
                                    (ok (and
                                         full-file
                                         (not inwindir)
                                         (file-executable-p full-file))))
                               (if ok
                                   (emacsw32-custom-valfaced image-dired-cmd-create-thumbnail-program)
                                 (concat image-dired-cmd-create-thumbnail-program
                                         "\n    "
                                         sError
                                         (if inwindir
                                             ": points to Windows convert.exe"
                                           (if (and full-file (file-exists-p full-file))
                                               ": File not executable"
                                             ": File not found"))
                                         )))))
                       "\n  "
                       "Current create temp image program: "
                       (if (setup-helper-customizeable 'image-dired-cmd-create-temp-image-program)
                           (if (equal image-dired-cmd-create-temp-image-program "")
                               (emacsw32-custom-valfaced "(none)")
                             (let* ((full-file
                                     (if (file-exists-p image-dired-cmd-create-temp-image-program)
                                         image-dired-cmd-create-temp-image-program
                                       (executable-find image-dired-cmd-create-temp-image-program)))
                                    (windir (file-name-directory (executable-find "notepad.exe")))
                                    (inwindir (and full-file
                                                   (string= windir
                                                            (file-name-directory full-file))))
                                    (ok (and
                                         full-file
                                         (not inwindir)
                                         (file-executable-p full-file))))
                               (if ok
                                   (emacsw32-custom-valfaced image-dired-cmd-create-temp-image-program)
                                 (concat image-dired-cmd-create-temp-image-program
                                         "\n    "
                                         sError
                                         (if inwindir
                                             ": points to Windows convert.exe"
                                           (if (and full-file (file-exists-p full-file))
                                               ": File not executable"
                                             ": File not found"))
                                         )))))
                       "\n  "
                       "Current create rotate image program: "
                       (if (setup-helper-customizeable 'image-dired-cmd-rotate-thumbnail-program)
                           (if (equal image-dired-cmd-rotate-thumbnail-program "")
                               (emacsw32-custom-valfaced "(none)")
                             (let* ((full-file
                                     (if (file-exists-p image-dired-cmd-rotate-thumbnail-program)
                                         image-dired-cmd-rotate-thumbnail-program
                                       (executable-find image-dired-cmd-rotate-thumbnail-program)))
                                    (ok (and
                                         full-file
                                         (file-executable-p full-file))))
                               (if ok
                                   (emacsw32-custom-valfaced image-dired-cmd-rotate-thumbnail-program)
                                 (concat image-dired-cmd-rotate-thumbnail-program
                                         "\n    "
                                         sError
                                         (if (and full-file (file-exists-p full-file))
                                             ": File not executable"
                                           ": File not found")
                                         )))))
                       "\n"
                       ))
           )

      (when updated
        (if error
            (progn
              (widget-insert "*")
              (widget-insert sError)
              (widget-insert (concat "** Could not update " updated ": " error "\n\n")))
          (widget-insert (concat "*** Updated " updated ". ***\n\n"))
          ))

      (emacsw32-custom-h1 (concat "Customize EmacsW32 ("
                                  (emacsw32-get-version)
                                  ")")
                          t)
      (widget-insert "

These settings are selected for making Emacs feel like a w32 program.
\(See ")

      (emacsw32-custom-url-link "EmacsW32 - Adjustment for Emacs on MS Windows" emacsw32-doc-file)

      (widget-insert ".)\n\n"
                     (propertize
                      "  Note that most of these features belongs to nXhtml, see below.\n\n"
                      'face '(:weight bold)))
      (emacsw32-custom-insert-emacsw32-row 'cua-mode t "W32 style keys: C-c, C-x, C-v, C-z; region visible")
      ;;(emacsw32-custom-insert-emacsw32-row 'emacsw32-mode t "More w32 style keys: C-a etc")
;      (emacsw32-custom-insert-emacsw32-row 'rebind-keys-mode t "More w32 style keys: C-a etc")
      (widget-insert "\n")
      (when (featurep 'menuacc)
        (emacsw32-custom-insert-emacsw32-row 'menuacc-mode t "Underlined accelerators in menu bar"))
      (when (featurep 'w32-meta)
        (emacsw32-custom-insert-emacsw32-row 'w32-meta-style 'w32-lr "Use keyboard Window keys as Emacs META"))
      (emacsw32-custom-insert-emacsw32-row 'recentf-mode t "Recent files list on File menu")
      (emacsw32-custom-insert-emacsw32-row 'appmenu-mode t "Popup menu on <apps>, Mouse-3")
      (widget-insert "\n")
      (emacsw32-custom-insert-emacsw32-row 'ourcomments-ido-ctrl-tab t "Simple buffer switching, default C-tab")
      (emacsw32-custom-insert-emacsw32-row 'tabkey2-mode t "Easy tab completion everywhere")
      (widget-insert "\n")
      (emacsw32-custom-insert-emacsw32-row 'emacsw32-style-frame-title t "W32 style frame title")
      (widget-insert "\n")
      (emacsw32-custom-insert-emacsw32-row 'hfyview-quick-print-in-files-menu t "Add quick print to File menu")
      (emacsw32-custom-insert-emacsw32-row 'noprint-hide-print-in-menus t "  Remove default print entries in File menu")
      (emacsw32-custom-insert-emacsw32-row 'noprint-hide-ps-print-in-menus t "  Remove default ps print entries in File menu")
      (widget-insert "\n")
      (emacsw32-custom-insert-emacsw32-row 'sex-mode t "Open binary files in default apps")
      (emacsw32-custom-insert-emacsw32-row 'w32shell-shell 'cmd "Inferior shell + path for unix style programs")
      ;;(emacsw32-custom-insert-emacsw32-row 'emacsw32-max-frames t "Maximize new frames")
      ;;(emacsw32-custom-insert-emacsw32-row 'swbuff-y-mode t "Simple buffer switching, default C-tab")

      (widget-insert "\n  ")
      (widget-create
       'push-button
       :notify (lambda (&rest ignore)
                 (emacsw32-custom-set 'cua-mode t)
                 ;;(emacsw32-custom-set 'emacsw32-mode t)
                 (emacsw32-custom-set 'rebind-keys-mode t)
                 (emacsw32-custom-set 'recentf-mode t)
                 (emacsw32-custom-set 'emacsw32-style-frame-title t)
                 ;;(emacsw32-custom-set 'swbuff-y-mode t)
                 (emacsw32-custom-set 'ourcomments-ido-ctrl-tab t)
                 ;;(emacsw32-custom-set 'emacsw32-max-frames t)
                 (emacsw32-custom-set 'hfyview-quick-print-in-files-menu t)
                 (emacsw32-custom-set 'noprint-hide-print-in-menus t)
                 (emacsw32-custom-set 'noprint-hide-ps-print-in-menus t)
                 (when (featurep 'w32-meta)
                   (emacsw32-custom-set 'w32-meta-style 'w32-lr))
                 (when (featurep 'menuacc)
                   (emacsw32-custom-set 'menuacc-mode t))
                 (emacsw32-custom-set 'w32shell-shell 'cmd)
                 (emacsw32-custom-set 'appmenu-mode t)
                 (emacsw32-custom-set 'tabkey2-mode t)
                 (emacsw32-custom-set 'sex-mode t)
                 (emacsw32-custom-save-all)
                 (emacsw32-custom-start))
       " Set all to EmacsW32 style! ")
      (widget-insert " ")
      (widget-create
       'push-button
       :notify (lambda (&rest ignore)
                 (setup-helper-erase-customization 'cua-mode)
                 ;;(setup-helper-erase-customization 'emacsw32-mode)
                 (setup-helper-erase-customization 'rebind-keys-mode)
                 (setup-helper-erase-customization 'recentf-mode)
                 (setup-helper-erase-customization 'emacsw32-style-frame-title)
                 ;;(setup-helper-erase-customization 'swbuff-y-mode)
                 (setup-helper-erase-customization 'ourcomments-ido-ctrl-tab)
                 (setup-helper-erase-customization 'emacsw32-max-frames)
                 (setup-helper-erase-customization 'hfyview-quick-print-in-files-menu)

                 (setup-helper-erase-customization 'noprint-hide-print-in-menus)
                 (setup-helper-erase-customization 'noprint-hide-ps-print-in-menus)
                 (when (featurep 'w32-meta)
                   (setup-helper-erase-customization 'w32-meta-style))
                 (when (featurep 'menuacc)
                   (setup-helper-erase-customization 'menuacc-mode))
                 (setup-helper-erase-customization 'w32shell-shell)
                 (setup-helper-erase-customization 'appmenu-mode)
                 (setup-helper-erase-customization 'tabkey2-mode)
                 (setup-helper-erase-customization 'sex-mode)
                 (emacsw32-custom-save-all)
                 (emacsw32-custom-start))
       " Reset all to default! ")
      (widget-insert " ")
      (widget-create 'push-button
                     :notify (lambda (&rest ignore)
                               (kill-buffer (current-buffer))
                               (customize-group 'emacsw32))
                     " Customize EmacsW32 ... ")
      (widget-insert "\n")

      (widget-insert "

EmacsW32 also has a bit enhanced support for handling line
endings.  This lets you tell which files should have LF line
endings (instead of CR-LF) based on the file names.

  ")
      (widget-create 'push-button
                     :notify (lambda (&rest ignore)
                               (kill-buffer (current-buffer))
                               (customize-group 'emacsw32-eol))
                     " Customize line endings ... ")

      (widget-insert "\n")

      (emacsw32-custom-h1 "Program and Value Search" t t)

      (widget-insert "

When you use Emacs on MS Windows you sometimes want to fetch values
and program locations from MS Windows.  Many of these values are
stored in the MS Windows Registry.  Since Emacs is written to be used
on many platforms \(with the emphasis on GPL platforms\) the effort to
let Emacs read the Registry directly has not been made.  Below you can
however let Emacs \(with the help of an external program\) search your
pc for some programs and values.

To change a specific program or value press 'Find!' to let Emacs
search and 'Customize!' to enter a value yourself.

To search for all known programs and values click 'Find All!' below.")

      (widget-insert "\n\n  ")
      (widget-create 'push-button
                     :notify (lambda (&rest ignore)
                               (message "Searching for all known values...")
                               (w32-regdat-addcust-all)
                               (emacsw32-find-spell-program)
                               ;;(custom-save-all)
                               (emacsw32-custom-start "all known values"))
                     " Find All! ")
      (widget-insert "\n\n")

      (let ((s "\nPrint related settings:\n"))
        (put-text-property 0 (length s)
                       'face '(bold
                               (foreground-color . "maroon")) s)
        (widget-insert s))

      (widget-insert "  ")
      (widget-insert
       (propertize
        "Note: If you use quick print above these settings are not needed.\n\n"
        'face '(:slant italic)))
      (widget-insert "  ")
      (widget-insert gs-msg)


      (when (setup-helper-customizeable 'pr-gs-command)
        (widget-insert "    ")
        (widget-create
         'push-button
         :notify (lambda (&rest ignore)
                   (message "Searching for GhostScript program...")
                   (let ((gs (w32-regdat-find-gs)))
                     (when gs
                       (emacsw32-custom-set 'pr-gs-command gs)
                       (emacsw32-custom-save-all))
                     (emacsw32-custom-start "pr-gs-command"
                                            (unless gs "Not found"))))
         " Find! ")
        (widget-insert " ")
        (widget-create 'push-button
                       :notify (lambda (&rest ignore)
                                 (kill-buffer (current-buffer))
                                 (customize-variable 'pr-gs-command))
                       " Customize... ")
        (widget-insert "\n"))
      (widget-insert "\n")

      (widget-insert "  ")
      (widget-insert gv-msg)
      (when (setup-helper-customizeable 'pr-gv-command)
        (widget-insert "    ")
        (widget-create
         'push-button
         :notify (lambda (&rest ignore)
                   (message "Searching for GSview program...")
                   (let ((gv (w32-regdat-find-gv)))
                     (when gv
                       (emacsw32-custom-set 'pr-gv-command gv)
                       (emacsw32-custom-save-all))
                     (emacsw32-custom-start "pr-gv-command"
                                            (unless gv
                                              "Not found"))))
         " Find! ")
        (widget-insert " ")
        (widget-create 'push-button
                       :notify (lambda (&rest ignore)
                                 (kill-buffer (current-buffer))
                                 (customize-variable 'pr-gv-command))
                       " Customize... ")
        (widget-insert "\n"))
      (widget-insert "\n")

      (when (setup-helper-customizeable 'ps-paper-type)
        (widget-insert "  ")
        (widget-insert paper-msg)
        (widget-insert "    ")
        (widget-create
         'push-button
         :notify (lambda (&rest ignore)
                   (message "Searching for paper size...")
                   (let ((paper (w32-regdat-get-paper-size)))
                     (when paper
                       (emacsw32-custom-set 'ps-paper-type (read (format "(quote %s)" paper)))
                       (emacsw32-custom-save-all))
                     (emacsw32-custom-start "ps-paper-type"
                                            (unless paper "Not found"))))
         " Find! ")
        (widget-insert " ")
        (widget-create 'push-button
                       :notify (lambda (&rest ignore)
                                 (kill-buffer (current-buffer))
                                 (customize-variable 'ps-paper-type))
                       " Customize... ")
        (widget-insert "\n"))
      (widget-insert "\n")


      (let ((s "\nInferior shells related settings:\n"))
        (put-text-property 0 (length s)
                       'face '(bold
                               (foreground-color . "maroon")) s)
        (widget-insert s))

      (when (setup-helper-customizeable 'w32shell-shell)
        (widget-insert "  ")
        (if w32shell-shell
            (let ((shell (format "%s" w32shell-shell))
                  (missing (w32shell-get-missing-progs)))
              (put-text-property 0 (length shell)
                                 'face '(bold) shell)
              (widget-insert "You are currently using "
                             shell
                             " for inferior shells.")
              (when (eq 'cmd w32shell-shell)
                (widget-insert
                 "\n  "
                 "This uses the unix progs that comes with EmacsW32."))
              (when missing
                (widget-insert
                 "\n    "
                 sError
                 " - "
                 "The following programs seems missing then:"
                 (format "\n    %s" missing))))
          (widget-insert "To take advantage of EmacsW32 handling of inferior shells"
                         "\n  you must customize w32shell and set at least w32shell-shell."))
        (widget-insert "\n    ")
        (widget-create 'push-button
                       :notify (lambda (&rest ignore)
                                 (kill-buffer (current-buffer))
                                 (customize-group 'w32shell))
                       " Customize w32shell ... ")
        (widget-insert "\n"))

      (when (setup-helper-customizeable 'w32shell-cygwin-bin)
        (widget-insert "\n  ")
        (widget-insert cygwin-msg)
        (widget-insert "    ")
        (widget-create 'push-button
                       :notify 'emacsw32-cygwin-find-notify
                       " Find! ")
        (widget-insert " ")
        (widget-create 'push-button
                       :notify (lambda (&rest ignore)
                                 (kill-buffer (current-buffer))
                                 (customize-variable 'w32shell-cygwin-bin))
                       " Customize... ")
        (widget-insert "\n"))

      (when (setup-helper-customizeable 'w32shell-msys-bin)
        (widget-insert "\n  ")
        (widget-insert msys-msg)
        (widget-insert "    ")
        (widget-create
         'push-button
         :notify (lambda (&rest ignore)
                   (message "Searching for MSYS...")
                   (let ((msys (w32-regdat-msys-bindir)))
                     (when msys
                       (emacsw32-custom-set 'w32shell-msys-bin msys)
                       (emacsw32-custom-save-all))
                     (emacsw32-custom-start "w32shell-msys-bin"
                                            (unless msys "Not found"))))
         " Find! ")
        (widget-insert " ")
        (widget-create 'push-button
                       :notify (lambda (&rest ignore)
                                 (kill-buffer (current-buffer))
                                 (customize-variable 'w32shell-msys-bin))
                       " Customize... ")
        (widget-insert "\n"))


      (widget-insert (propertize "\n\nSpelling:\n"
                                 'face '(bold
                                         (foreground-color . "maroon"))))
      (when (setup-helper-customizeable 'ispell-program-name)
        (widget-insert "  ")
        (widget-insert spell-msg)
        (widget-insert "    ")
        (widget-create
         'push-button
         :notify (lambda (&rest ignore)
                   (message "Searching for spell program...")
                   (let ((spell-prog (emacsw32-find-spell-program)))
                     (emacsw32-custom-start "ispell-program-name"
                                            (unless spell-prog "Not found"))))
         " Find! ")
        (widget-insert " ")
        (widget-create 'push-button
                       :notify (lambda (&rest ignore)
                                 (kill-buffer (current-buffer))
                                 (customize-variable 'ispell-program-name))
                       " Customize... ")
        (widget-insert "\n"))


      (let ((s "\n\nimage-dired related settings:\n"))
        (put-text-property 0 (length s)
                       'face '(bold
                               (foreground-color . "maroon")) s)
        (widget-insert s))

      (when (setup-helper-customizeable 'image-dired-cmd-create-thumbnail-program)
        (widget-insert "  ")
        (widget-insert image-dired-msg)
        (widget-insert "    ")
        (widget-create
         'push-button
         :notify (lambda (&rest ignore)
                   (message "Searching for ImageMagick...")
                   (let ((bindir (w32-regdat-imagemagick-bindir)))
                     (when bindir
                       (let* ((create-prog (expand-file-name "convert.exe" bindir))
                              (tempimg-prog create-prog)
                              (rotate-prog (expand-file-name "mogrify.exe" bindir)))
                         (emacsw32-custom-set 'image-dired-cmd-create-thumbnail-program create-prog)
                         (emacsw32-custom-set 'image-dired-cmd-create-temp-image-program tempimg-prog)
                         (emacsw32-custom-set 'image-dired-cmd-rotate-thumbnail-program rotate-prog)
                         (emacsw32-custom-save-all)))
                     (emacsw32-custom-start (concat
                                             "image-dired-cmd-create-thumbnail-program"
                                             " image-dired-cmd-create-temp-image-program"
                                             " image-dired-cmd-rotate-thumbnail-program")
                                            (unless bindir "Not found"))))
         " Find! ")
        (widget-insert " ")
        (widget-create 'push-button
                       :notify (lambda (&rest ignore)
                                 (kill-buffer (current-buffer))
                                 (customize-group 'image-dired))
                       " Customize image-dired... ")
        (widget-insert "\n"))





      (emacsw32-custom-h1 "Unix Utilities for Emacs on MS Windows" t t)

      (widget-insert "

Please see ")
      (emacsw32-custom-url-link
       "Unix Utilities for Emacs on MS Windows"
       (convert-standard-filename
        (expand-file-name (concat +exec-directory+
                                  "../../EmacsW32/etc/w32-unix-progs.html"))))
      (widget-insert
" for information about how
to find ports to MS Windows of unix utilities and other programs you may
want when you use Emacs.

Please notice that this Emacs+EmacsW32 comes with some GNU
utilities, like grep, diff, patch, xargs, cmp etc that Emacs
needs for some commands \(for example the very useful
ediff-buffers).  Those utilities are already setup for you, you
do not have to do anything - which you otherwise would have to do
on Windows \(but not on for example GNU/Linus).")



      (emacsw32-custom-h1 "Bonus: The nXhtml multi-purpose package" t t)

      (widget-insert "

I have included nXhtml in the EmacsW32 distribution.  It is a
package for multi major mode editing that is good for things like
php files.  It got its name from a major part of it, nxhtml-mode,
for editing XHTML files.  However it is not only for that and the
package contains some other goodies that I do not have time to
distribute another way \(like for example an n-back game...).

For more information please see ")

      ;;(load-library "../nxhtml/nxhtml/nxhtml.el")
      (require 'nxhtml nil t)
      (require 'nxhtml-mode nil t)
      (unless (and (fboundp 'nxhtml-mode)
                   (not (eq 'autoload
                            (car-safe (symbol-function 'nxhtml-mode)))))
        (with-temp-buffer
           (insert-file
            (let* ((ew32dir (expand-file-name "../../emacsw32" +exec-directory+))
                   (nxhtml-el (expand-file-name "nxhtml/nxhtml/nxhtml-mode.el" ew32dir)))
              nxhtml-el))
          (search-forward "(define-derived-mode nxhtml-mode")
          (eval-defun nil)))
      ;;(emacsw32-custom-describe-symbol "nxhtml-mode" 'nxhtml-mode)
    (widget-create 'push-button
                   ;;:notify 'emacsw32-notify-btn-url
                   :notify (lambda (&rest ignore)
                             "Show nXhtml overview in web broswer"
                             (nxhtml-overview))
                   :button-face 'emacsw32-link-face
                   :mouse-face 'highlight
                   :button-prefix ""
                   :button-suffix ""
                   "nXhtml overview")

      (widget-insert ".

To setup Emacs to load nXhtml when using EmacsW32 you can
customize nxhtml-load")
      (if nxhtml-load
          (insert " (nXhtml is already loaded this way)")
        (insert " (nXhtml is not loaded this way now)"))
      (widget-insert ":
    ")

        (widget-create 'push-button
                       :notify (lambda (&rest ignore)
                                 (kill-buffer (current-buffer))
                                 (customize-option 'nxhtml-load))
                       " Customize nxhtml-load ... ")

        ;; (let ((html-major (with-temp-buffer
        ;;                     (setq buffer-file-name "temp.html")
        ;;                     (set-auto-mode t)
        ;;                     (or (when (boundp 'mumamo-multi-major-mode)
        ;;                           mumamo-multi-major-mode)
        ;;                         major-mode)))
        ;;       (xml-major (with-temp-buffer
        ;;                    (setq buffer-file-name "temp.xml")
        ;;                    (set-auto-mode t)
        ;;                    (or (when (boundp 'mumamo-multi-major-mode)
        ;;                          mumamo-multi-major-mode)
        ;;                        major-mode))))
        ;;   (insert (format "\nYou are currently using %s for XML files\n" xml-major)
        ;;           (format "and %s for HTML files." html-major)))

      (buffer-disable-undo)
      ;;(use-local-map widget-keymap) ; Using this disables mouse clicks???
      (widget-setup)
      (buffer-enable-undo)
      (message "")
      (goto-char (point-min))
      (goto-char 0)
      )))



;; dired.el:(defcustom dired-free-space-program "df" gnuwin32 FileUtils
;; ediff-diff.el:(defcustom ediff-diff-program "diff" gnuwin32 DiffUtils
;; ediff-diff.el:(defcustom ediff-diff3-program "diff3" gnuwin32 DiffUtils
;; ediff-diff.el:(defcustom ediff-cmp-program "cmp" gnuwin32 DiffUtils
;; ediff-diff.el:(defcustom ediff-custom-diff-program ediff-diff-program gnuwin32 DiffUtils
;; ediff-ptch.el:(defcustom ediff-patch-program  "patch" gnuwin32 Patch
;; emerge.el:(defcustom emerge-diff-program "diff"
;; emerge.el:(defcustom emerge-diff3-program "diff3"
;; emerge.el:(defcustom emerge-rcs-ci-program "ci" cygwin
;; emerge.el:(defcustom emerge-rcs-co-program "co" cygwin
;; find-dired.el:(defcustom find-dired-find-program "find" gnuwin32 FindUtils
;; gnus\imap.el:(defcustom imap-kerberos4-program '("imtest -m kerberos_v4 -u %l -p %p %s"
;; gnus\imap.el:(defcustom imap-gssapi-program '("imtest -m gssapi -u %l -p %p %s")
;; gnus\imap.el:(defcustom imap-ssl-program '("openssl s_client -ssl3 -connect %s:%p"
;; gnus\imap.el:(defcustom imap-shell-program '("ssh %s imapd" Putty?
;; gnus\message.el:(defcustom message-qmail-inject-program "/var/qmail/bin/qmail-inject"
;; gnus\starttls.el:(defcustom starttls-program "starttls"
;; gnus\uudecode.el:(defcustom uudecode-decoder-program "uudecode" gnuwin32 UUEncode
;; hexl.el:(defcustom hexl-program "hexl" ??
;; lpr.el:(defcustom lpr-page-header-program "pr" (none)
;; ls-lisp.el:(defcustom ls-lisp-use-insert-directory-program nil
;; mail\metamail.el:(defcustom metamail-program-name "metamail"
;; mail\rmail.el:(defcustom rmail-movemail-program nil
;; net\ange-ftp.el:(defcustom ange-ftp-gateway-program-interactive nil
;; net\ange-ftp.el:(defcustom ange-ftp-gateway-program remote-shell-program
;; net\ange-ftp.el:(defcustom ange-ftp-ftp-program-name "ftp"
;; net\ange-ftp.el:(defcustom ange-ftp-gateway-ftp-program-name "ftp"
;; net\ange-ftp.el:(defcustom ange-ftp-ftp-program-args '("-i" "-n" "-g" "-v")
;; net\ange-ftp.el:(defcustom ange-ftp-nslookup-program nil
;; net\browse-url.el:(defcustom browse-url-netscape-program "netscape"
;; net\browse-url.el:(defcustom browse-url-mosaic-program "xmosaic"
;; net\browse-url.el:(defcustom browse-url-xterm-program "xterm"
;; net\browse-url.el:(defcustom browse-url-gnudoit-program "gnudoit"
;; net\browse-url.el:(defcustom browse-url-generic-program nil
;; net\browse-url.el:(defcustom browse-url-kde-program "kfmclient"
;; net\net-utils.el:(defcustom traceroute-program
;; net\net-utils.el:(defcustom traceroute-program-options nil
;; net\net-utils.el:(defcustom ping-program "ping" OK?
;; net\net-utils.el:(defcustom ping-program-options
;; net\net-utils.el:(defcustom ipconfig-program OK?
;; net\net-utils.el:(defcustom ipconfig-program-options
;; net\net-utils.el:(defcustom netstat-program  "netstat" OK?
;; net\net-utils.el:(defcustom netstat-program-options
;; net\net-utils.el:(defcustom arp-program  "arp" ???
;; net\net-utils.el:(defcustom arp-program-options
;; net\net-utils.el:(defcustom route-program OK???
;; net\net-utils.el:(defcustom route-program-options
;; net\net-utils.el:(defcustom nslookup-program  "nslookup" OK
;; net\net-utils.el:(defcustom nslookup-program-options  nil
;; net\net-utils.el:(defcustom dig-program  "dig" http://pigtail.net/LRP/dig/
;; net\net-utils.el:(defcustom ftp-program "ftp" OK?
;; net\net-utils.el:(defcustom ftp-program-options nil
;; net\net-utils.el:(defcustom smbclient-program "smbclient"
;; net\net-utils.el:(defcustom smbclient-program-options nil
;; net\rlogin.el:(defcustom rlogin-program "rlogin" putty
;; play\fortune.el:(defcustom fortune-program "fortune"
;; play\fortune.el:(defcustom fortune-program-options ""
;; progmodes\cperl-mode.el:(defcustom pod2man-program "pod2man" ??
;; progmodes\f90.el:(defcustom f90-program-indent 2
;; progmodes\m4-mode.el:(defcustom m4-program gnuwin32 M4
;; progmodes\m4-mode.el:(defcustom m4-program-options nil
;; progmodes\octave-inf.el:(defcustom inferior-octave-program "octave"
;; progmodes\prolog.el:(defcustom prolog-program-name "prolog"
;; progmodes\scheme.el:(defcustom scheme-program-name "scheme"
;; progmodes\sql.el:(defcustom sql-oracle-program "sqlplus"
;; progmodes\sql.el:(defcustom sql-mysql-program "mysql"
;; progmodes\sql.el:(defcustom sql-solid-program "solsql"
;; progmodes\sql.el:(defcustom sql-sybase-program "isql"
;; progmodes\sql.el:(defcustom sql-informix-program "dbaccess"
;; progmodes\sql.el:(defcustom sql-ingres-program "sql"
;; progmodes\sql.el:(defcustom sql-ms-program "isql"
;; progmodes\sql.el:(defcustom sql-postgres-program "psql"
;; progmodes\sql.el:(defcustom sql-interbase-program "isql"
;; progmodes\sql.el:(defcustom sql-db2-program "db2"
;; server.el:(defcustom server-program (expand-file-name "emacsserver" +exec-directory+)
;; textmodes\artist.el:(defcustom artist-figlet-program "figlet" ???
;; textmodes\ispell.el:(defcustom ispell-program-name "ispell"
;; xscheme.el:(defcustom scheme-program-arguments nil


;; E:\emacs-21.3\lisp>findstr /s defcustom.*-command *.el
;; diff.el:(defcustom diff-command "diff"
;; emulation\old-viper\viper-ex.el:(defcustom ex-compile-command "make"
;; emulation\viper-ex.el:(defcustom ex-compile-command "make"
;; eshell\esh-cmd.el:(defcustom eshell-debug-command nil
;; filecache.el:(defcustom file-cache-find-command "find"
;; filecache.el:(defcustom file-cache-locate-command "locate"
;; gnus\gnus-art.el:(defcustom gnus-article-x-face-command
;; gud.el:(defcustom gud-perldb-command-name "perl"
;; gud.el:(defcustom gud-pdb-command-name "pdb"
;; hexl.el:(defcustom hexlify-command
;; hexl.el:(defcustom dehexlify-command
;; locate.el:(defcustom locate-command "locate"
;; locate.el:(defcustom locate-update-command "updatedb"
;; locate.el:(defcustom locate-prompt-for-command nil
;; lpr.el:(defcustom lpr-command
;; mail\mailalias.el:(defcustom mail-passwd-command nil
;; mail\mh-e.el:(defcustom mh-lpr-command-format "lpr -J '%s'"
;; pcomplete.el:(defcustom pcomplete-command-completion-function
;; pcomplete.el:(defcustom pcomplete-command-name-function 'pcomplete-command-name
;; progmodes\antlr-mode.el:(defcustom antlr-tool-command "java antlr.Tool"
;; progmodes\compile.el:(defcustom grep-command nil
;; progmodes\compile.el:(defcustom grep-find-command nil
;; progmodes\compile.el:(defcustom compilation-read-command t
;; progmodes\compile.el:(defcustom compile-command "make -k "
;; progmodes\cperl-mode.el:(defcustom cperl-info-on-command-no-prompt nil
;; progmodes\dcl-mode.el:(defcustom dcl-calc-command-indent-function nil
;; progmodes\dcl-mode.el:(defcustom dcl-command-regexp
;; progmodes\idlw-shell.el:(defcustom idlwave-shell-initial-commands "!more=0"
;; progmodes\idlwave.el:(defcustom idlwave-shell-command-line-options nil
;; progmodes\modula2.el:(defcustom m2-compile-command "m2c"
;; progmodes\modula2.el:(defcustom m2-link-command "m2l"
;; progmodes\sh-script.el:(defcustom sh-beginning-of-command
;; progmodes\sh-script.el:(defcustom sh-end-of-command
;; progmodes\tcl.el:(defcustom tcl-command-switches nil
;; progmodes\tcl.el:(defcustom inferior-tcl-source-command "source %s\n"
;; ps-print.el:(defcustom ps-lpr-command lpr-command
;; recentf.el:(defcustom recentf-menu-append-commands-p t
;; shell.el:(defcustom shell-command-regexp "[^;&|\n]+"
;; simple.el:(defcustom read-mail-command 'rmail
;; smerge-mode.el:(defcustom smerge-command-prefix "\C-c^"
;; strokes.el:(defcustom strokes-click-command 'mouse-yank-at-click
;; textmodes\artist.el:(defcustom artist-figlet-list-fonts-command
;; textmodes\flyspell.el:(defcustom flyspell-default-delayed-commands
;; textmodes\flyspell.el:(defcustom flyspell-delayed-commands nil
;; textmodes\flyspell.el:(defcustom flyspell-default-deplacement-commands
;; textmodes\flyspell.el:(defcustom flyspell-deplacement-commands nil
;; textmodes\flyspell.el:(defcustom flyspell-tex-command-regexp
;; textmodes\flyspell.el:(defcustom flyspell-check-tex-math-command nil
;; textmodes\ispell.el:(defcustom ispell-grep-command "egrep"
;; textmodes\ispell.el:(defcustom ispell-look-command
;; textmodes\ispell.el:(defcustom ispell-look-p (file-exists-p ispell-look-command)
;; textmodes\makeinfo.el:(defcustom makeinfo-run-command "makeinfo"
;; textmodes\sgml-mode.el:(defcustom sgml-validate-command "nsgmls -s"  ; replaced old `sgml
;; textmodes\spell.el:(defcustom spell-command "spell"
;; textmodes\tex-mode.el:(defcustom tex-run-command "tex"
;; textmodes\tex-mode.el:(defcustom latex-run-command "latex"
;; textmodes\tex-mode.el:(defcustom slitex-run-command "slitex"
;; textmodes\tex-mode.el:(defcustom tex-bibtex-command "bibtex"
;; textmodes\tex-mode.el:(defcustom tex-dvi-print-command "lpr -d"
;; textmodes\tex-mode.el:(defcustom tex-alt-dvi-print-command "lpr -d"
;; textmodes\tex-mode.el:(defcustom tex-dvi-view-command nil
;; textmodes\tex-mode.el:(defcustom tex-show-queue-command "lpq"
;; textmodes\texinfo.el:(defcustom texinfo-texi2dvi-command "texi2dvi"
;; textmodes\texinfo.el:(defcustom texinfo-tex-command "tex"
;; textmodes\texinfo.el:(defcustom texinfo-texindex-command "texindex"
;; textmodes\texinfo.el:(defcustom texinfo-delete-from-print-queue-command "lprm"
;; vc.el:(defcustom vc-command-messages nil

;; ----------------------------
;; http://www.comp.lancs.ac.uk/~fittond/win32latex/win32latex.html
;; http://www.miktex.org/about.html
;; ----------------------------
;; http://gnuwin32.sourceforge.net/links.html#postscript_and_pdf
;; ----------------------------
;; lpr is available in
;; http://gnuwin32.sourceforge.net/packages/cygutils.htm
;; You have to download the whole package.
;; Compiling lpr with MinGW:
;; 0) Use MSYS sh - maybe place MSYS bin first in path.
;; 1) Run configure
;; 2) Do these small changes to lpr.cc:
;; ***** lpr.cc
;; /* #include <sys/cygwin.h> */
;; #include <_mingw.h>
;; ***** LPR.CC-ORIG
;; #include <sys/cygwin.h>
;; *****
;; ***** lpr.cc
;; char winPrinter[MAX_PATH] ;
;; //cygwin_conv_to_win32_path(printerName.c_str(), winPrinter) ;
;; strcpy(winPrinter, printerName.c_str() ) ;
;; if (debugFlag)
;; ***** LPR.CC-ORIG
;; char winPrinter[MAX_PATH] ;
;; cygwin_conv_to_win32_path(printerName.c_str(), winPrinter) ;
;; if (debugFlag)
;; *****
;; 3) make src/lpr/lpr.exe

(provide 'emacsw32-custom)

;;; emacsw32-custom.el ends here
