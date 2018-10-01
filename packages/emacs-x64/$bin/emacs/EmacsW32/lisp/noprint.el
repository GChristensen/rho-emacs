;;; noprint.el --- MS Windows printing
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2008-01-20
;; Version: 1.0
;; Last-Updated: 2008-01-20T02:05:16+0100 Sun
;; Keywords: extensions convenience printing
;; URL: http://OurComments.org/Emacs/
;; Features that might be required by this library:
;;
;;   None
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; With this file the default print entries in Emacs on the File menu
;; can be removed, see the settings `noprint-hide-ps-print-in-menus'
;; and `noprint-hide-print-in-menus'.  This is actually all you can do
;; with this file. See those settings for more info about why and how.
;;
;; To use this package put in your load-path and do
;;
;;    (require 'noprint)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; History:
;;
;; 2008-01-19:
;;   - Birth. Replaces part of w32-print.el

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This file is not part of GNU Emacs
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; Menus


(defun noprint-set-visible (menu entry visible)
  (let* ((pb (assoc entry menu))
         (hasvis (memq :visible pb))
         (helpcdr (cdr (memq :help pb))))
    (if hasvis
        (progn
          (setq hasvis (cdr hasvis))
          (setcar hasvis visible))
      (unless visible
        (setcdr helpcdr (cons :visible (cons nil (cdr helpcdr))))))))

(defun noprint-set-visible-ps-print-entries (visible)
  "Remove default ps-print entries from the file menu."
  ;;(message "ps-print-en %s" visible) (sit-for 4)
  (noprint-set-visible menu-bar-file-menu 'ps-print-buffer visible)
  (noprint-set-visible menu-bar-file-menu 'ps-print-region visible)
  (noprint-set-visible menu-bar-file-menu 'ps-print-buffer-faces visible)
  (noprint-set-visible menu-bar-file-menu 'ps-print-region-faces visible))

(defun noprint-set-visible-print-entries (visible)
  "Remove default print entries from file menu.
They are useless on MS Windows unless the user has added capabilities
for them."
  ;;(message "print-en %s" visible) (sit-for 4)
  (noprint-set-visible menu-bar-file-menu 'print-buffer visible)
  (noprint-set-visible menu-bar-file-menu 'print-region visible))

(defgroup noprint nil
  "Remove printing options normally unuseable on MS Windows."
  :tag "Printing setup"
  :group 'emacsw32)

(defcustom noprint-hide-print-in-menus nil
  "If non-nil remove default print entries from the file menu.
These may be useless and even in some cases hang Emacs completely
with no chance to save unsaved files on MS Windows unless you do
a sometimes rather complicated setup for printing.

`hfyview-buffer' in hfyview.el offer an alternative way to print
using your web browser."
  :tag "Remove default print entries from the file menu"
  :type 'boolean
  :set (lambda (symbol value)
         (set-default symbol value)
         (noprint-set-visible-print-entries (not value)))
  :group 'noprint
  )

(defcustom noprint-hide-ps-print-in-menus nil
  "If non-nil remove default ps-print entries from the file menu.
The reason to remove them has nothing do to with the ps-print-*
routines by themselves, but adequate printing routines has not
been implemented in Emacs on MS Windows. See
`noprint-menu-show-print' for more information."
  :tag "Remove default ps-print entries from the file menu"
  :type 'boolean
  :set (lambda (symbol value)
         (set-default symbol value)
         (noprint-set-visible-ps-print-entries (not value)))
  :group 'noprint
  )


(provide 'noprint)

;;; noprint.el ends here
