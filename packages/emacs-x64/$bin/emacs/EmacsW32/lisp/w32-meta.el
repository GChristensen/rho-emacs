;;; w32-meta.el --- Handling of Alt and Windows keys
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2005-07-30
;; Version: 0.50
;; Last-Updated: Tue Oct 30 18:29:33 2007 (3600 +0100)
;; Keywords: w32 keyboard


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This files requires that Emacs has the function
;; `w32-wh-keyboard-ll'. This is not yet part of Emacs but is included
;; in the patched Emacs available on EmacsW32 home page.
;;
;; This file makes it simple to setup the left and right windows keys
;; for use as meta and the Alt key for use by the menus.
;; For more info see `w32-meta-style'.
;;
;; Usage:
;;
;;    (require 'w32-meta)


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


(defun w32-meta-set-w32-style (lwindow-meta rwindow-meta)
  "Setup Emacs to feel more familiar to an MS Windows user.
Alt will be used for the menus.  If LWINDOW-META is non-nil then
the left Windows keyboard key will not be passed to MS Windows.
Instead it will be used as Meta in Emacs.  The same applies to
RWINDOW-META.

You can set this through custom with the variable
`w32-meta-style'.  See also `w32-meta-set-emacs-style'.

This is an high level interface to `w32-wh-keyboard-ll',
`w32-pass-lwindow-to-system', `w32-pass-rwindow-to-system',
`w32-pass-alt-to-system' and `w32-alt-is-meta'.

The `w32-wh-keyboard-ll' is not called unless `window-system' is
'w32."
  (interactive (list (y-or-n-p "Use left Windows key (lwindow) as Meta? ")
                     (y-or-n-p "Use right Windows key (rwindow) as Meta? ")))
  (and (eq window-system 'w32)
       (or lwindow-meta
           rwindow-meta)
       (unless (w32-wh-keyboard-ll t)
         (error "w32-wh-keyboard-ll failed add")))
  (when lwindow-meta
    ;;(global-set-key [(lwindow)] 'ESC-prefix)
    (setq w32-lwindow-modifier 'meta)
    (setq w32-pass-lwindow-to-system nil))
  (when rwindow-meta
    ;;(global-set-key [(rwindow)] 'ESC-prefix)
    (setq w32-rwindow-modifier 'meta)
    (setq w32-pass-rwindow-to-system nil))
  (setq w32-pass-alt-to-system t)
  (setq w32-alt-is-meta nil))

(defun w32-meta-set-emacs-style ()
  "Setup Emacs to be more familiar to a non MS Windows user.
Alt will be used for Emacs Meta.  <lwindow> and <rwindow> will be
passed to MS Windows.

See also `w32-meta-set-w32-style'."
  (interactive)
  (when (eq window-system 'w32)
    (unless (w32-wh-keyboard-ll nil)
      (error "w32-wh-keyboard-ll failed remove")))
  (global-set-key [(lwindow)] 'ignore)
  (global-set-key [(rwindow)] 'ignore)
  (setq w32-pass-lwindow-to-system t)
  (setq w32-pass-rwindow-to-system t)
  (setq w32-lwindow-modifier nil)
  (setq w32-rwindow-modifier nil)
  (setq w32-pass-alt-to-system nil)
  (setq w32-alt-is-meta t))


(defcustom w32-meta-style 'emacs
  "Defines how Emacs treats Alt, left and right Windows keys.
For more info see function `w32-meta-set-w32-style'."
  :group 'w32
  :group 'emacsw32
  :type '(choice
          (const :tag "Emacs default Alt handling+Windows keys default handling" emacs)
          (const :tag "MS Windows default Alt handling+left and right Windows key as Meta" w32-lr)
          (const :tag "MS Windows default Alt handling+left Windows key as Meta" w32-l)
          (const :tag "MS Windows default Alt handling+right Windows key as Meta" w32-r)
          (const :tag "MS Windows default Alt handling" w32))
  :set (lambda (symbol value)
         (unless (memq value '(emacs w32-lr w32-l w32-r w32))
           (error "Bad value to w32-meta-style"))
         (set-default symbol value)
         (cond
           ((eq value 'emacs) (w32-meta-set-emacs-style))
           ((eq value 'w32-lr) (w32-meta-set-w32-style t t))
           ((eq value 'w32-l)  (w32-meta-set-w32-style t nil))
           ((eq value 'w32-r)  (w32-meta-set-w32-style nil t))
           ((eq value 'w32)    (w32-meta-set-w32-style nil nil)))))

(defun w32-meta-lr ()
  (w32-meta-set-w32-style t t))

;;(w32-meta-set-w32-style t nil)

(provide 'w32-meta)

;;; w32-meta.el ends here
