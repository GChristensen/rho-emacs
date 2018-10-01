;;; emacsw32-setup-custom.el --- Functions for installation and setup of Emacs
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2004-09-24
;; Last-Updated: Mon Jan 15 03:08:08 2007 (3600 +0100)
;; Version: 1.0
;; Keywords: installation setup
;; Features that might be required by this library:

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

;; This file is used to setup customizations in Emacs. It is used by
;; Emacs MS Windows Setup Helper.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; History:
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

;;(add-to-list 'load-path (expand-file-name (concat +exec-directory+ "../../EmacsW32/lisp/inst/")))
(add-to-list 'load-path (expand-file-name (concat +exec-directory+ "../../EmacsW32/lisp/")))

(require 'setup-helper)


(defun emacsw32-add-custom()
  "Adds default custom values for emacsw32 related variables."
  ;;(setup-helper-add-custom 'global-font-lock-mode t nil 'font-lock)
  ;;(setup-helper-add-custom 'swbuff-clear-delay nil '0)
  (setup-helper-add-custom 'swbuff-exclude-buffer-regexps nil '(quote ("^ " "^*.**")))
  ;; Use the values written by Setup Helper:
  (let ((setuptemp-el (expand-file-name
		       (concat +exec-directory+ "../../EmacsW32/tmp/SetupTemp.el"))))
    (when (file-exists-p setuptemp-el) (load-file setuptemp-el)))
  (custom-save-all) )

;; Do it!
(emacsw32-add-custom)

(provide 'emacsw32-setup-custom)

;;; emacsw32-setup-custom.el ends here
