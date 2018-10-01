;;; nxhtml-loader.el --- Loading of nxhtml/nxml
;;
;; Author: Lennart Borgman
;; Maintainer:
;; Created: Fri Dec 15 16:56:15 2006
;; Version:
;; Last-Updated: 2008-01-20T01:20:56+0100 Sun
;; Keywords:
;; Compatibility:
;;
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(defconst nxhtml-loader-dir (file-name-directory
                             (if load-file-name load-file-name buffer-file-name)))

(defun nxhtml-load-nxhtml-from-emacsw32()
  "Load nxml-mode and nxhtml-mode.
Those are both loaded from the EmacsW32 installation which comes
with those."
  (unless (fboundp 'nxhtml-mode)
    (let* ((nxhtml-dir (file-name-as-directory
                      (expand-file-name "../nxhtml" nxhtml-loader-dir)))
           (auto (expand-file-name "autostart" nxhtml-dir)))
      (if (not (file-directory-p nxhtml-dir))
          (lwarn '(nxhtml) :error
                 "%s is not a directory, could not load nxhtml." nxhtml-dir)
        (load auto)))))

(defcustom nxhtml-load nil
  "Set this on if you want load nXhtml when starting Emacs.
Loading nXhtml will change the default major mode for some files.

For more information about this and the possibilities nXhtml
gives please see the overview.

Note: This way of loading nXhtml is specific to EmacsW32!"
  ;; Fix-me: Something better to do when resetting?
  :type 'boolean
  :set (lambda (symbol value)
         (set-default symbol value)
         (when value
           (message "Loading nxhtml from emacsw32...")
           (nxhtml-load-nxhtml-from-emacsw32)))
  :group 'xml
  :group 'nxhtml
  :group 'languages
  :group 'emacsw32
  :set-after '(
               ;; All defvars from nxhtml-loaddefs.el
               nxhtml-menu-mode
               nxml-where-global-mode
               csharp-mode-hook
               global-mozadd-refresh-edited-on-save-mode
               global-mozadd-mirror-mode
               php-file-patterns
               appmenu-mode
               as-external-mode
               css-color-global-mode
               css-palette-global-mode
               fold-dwim-mode
               foldit-global-mode
               hfyview-quick-print-in-files-menu
               hl-needed-mode
               inlimg-global-mode
               markchars-global-mode
               mlinks-global-mode
               better-fringes-mode
               ourcomments-ido-ctrl-tab
               ourcomments-paste-with-convert-mode
               ourcomments-M-x-menu-mode
               pause-mode
               global-pointback-mode
               popcmp-completion-style
               rebind-keys-mode
               sex-mode
               sml-modeline-mode
               tabkey2-mode
               tyda-mode
               vline-global-mode
               winsav-save-mode
               wrap-to-fill-left-marg
               wrap-to-fill-left-marg-modes
               ))

(provide 'nxhtml-loader)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; nxhtml-loader.el ends here
