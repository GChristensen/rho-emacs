;;; emacsw32-eol.el --- End of line choice for new files
;;
;; Filename: emacsw32-eol.el
;; Author: Lennart Borgman
;; Maintainer:
;; Created: Fri Jun 16 19:26:38 2006
;; Version: 0.55
;; Last-Updated: 2009-10-26 Mon
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
;; This file tries to make the handling of LF/CR-LF a bit easier by
;; enhancing the functions in dos-w32.el. It adds the possibility to
;; tell which file should have LF line endings based on file name
;; extension. It can also check LF line endings before saving a file.
;;
;; Using this may make it easier to upload files to transfer files to
;; a unix server (which may be a web server) using tramp in Emacs
;; since this normally uses binary file transfers. However it may make
;; it harder to use certain programs to edit those files locally if
;; you are on MS Windows. At the moment I am only aware that this is a
;; problem with Notepad, but I have heard there are other programs who
;; are problematic.
;;
;; To use it load the library:
;;
;;     (require 'emacsw32-eol)
;;
;; and then set the custom variables `emacsw32-eol-file-name-lf-list',
;; `emacsw32-eol-check-new-files' and `emacsw32-eol-check-before-save'.
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

(defun emacsw32-eol-file-name-lf (filename)
  (let ((fs (untranslated-canonical-name filename))
        (lf-list emacsw32-eol-file-name-lf-list)
        found)
    (while (and (not found) lf-list)
      (if (string-match (car lf-list) fs)
	  (setq found t)
	(setq lf-list (cdr lf-list))))
    found))

(defun emacsw32-eol-lf-preferred (filename)
  "Return t if file should have LF file endings when created."
  (or (emacsw32-eol-file-name-lf filename)
      (untranslated-file-p filename)))

(defun emacsw32-eol-find-file-not-found ()
  "Set end of line for new files.
If `buffer-file-name' matches `untranslated-file-p' or in
`emacsw32-eol-file-name-lf-alist' then unix style line endings is
used.

However if `coding-system-for-read' or `inhibit-eol-conversion'
is non-nil then they are obeyed instead.

This is meant to superseed
`find-file-not-found-set-buffer-file-coding-system'."
  (emacsw32-eol-set nil))

(defcustom emacsw32-eol-ask-before-save t
  "Ask before changing line end style if non-nil."
  :type 'boolean
  :group 'emacsw32-eol)

(defun emacsw32-eol-before-save ()
  "Set end of line before saving files.
See `emacsw32-eol-find-file-not-found' for more information."
  (when emacsw32-eol-check-before-save
    (emacsw32-eol-set emacsw32-eol-ask-before-save)))

(defun emacsw32-eol-set (ask)
  (let ((coding buffer-file-coding-system)
        new-coding)
    (if (eq coding 'no-conversion)
        (setq buffer-file-type t)
      (when (and (null coding-system-for-read)
		 (or inhibit-eol-conversion
                     (emacsw32-eol-lf-preferred (buffer-file-name))))
	(setq new-coding (coding-system-change-eol-conversion coding 0))
        (unless (equal coding new-coding)
          (let* ((old-eol-type (coding-system-eol-type coding))
                 (prompt
                  (concat
                   "Your preferred line ending for this file is unix style,\n"
                   "but current line endings is "
                   (cond ((vectorp old-eol-type) "unknown")
                         ((= old-eol-type 0) "unix")
                         ((= old-eol-type 1) "dos")
                         ((= old-eol-type 2) "mac")
                         (t "unknown"))
                   " style.\n\nChange to unix style line endings? ")))
            (when (or (not ask)
                      (y-or-n-p prompt))
              (setq buffer-file-coding-system new-coding))))))))


(defgroup emacsw32-eol nil
  "Checking that end of line is unix for some files."
  :group 'w32
  :group 'emacsw32)

(defcustom emacsw32-eol-file-name-lf-list
  ;; More info needed about files that should have LF line endings!!
  '(
    ;; Web:
    "\.x?html?$"
    "\.css$"
    "\.js$" ;; Is this correct? Does it work for jscript?
    ;; Perl:
    "\.pl$"
    "\.pm$"
    ;; Emacs:
    "\.el$"
    "\.texi$"
    )
  "Reg exps telling which files should have LF line endings.
If a file name matches any of the regular expressions in the list
then when creating new files it will normally get unix style (LF)
line endings."
  :type '(repeat
          (regexp
           :tag "File name regexp"))
  :group 'emacsw32-eol)

(defcustom emacsw32-eol-check-new-files nil
  "If non-nil set end of line for new files to 'unix.
This adheres to files matching `emacsw32-eol-file-name-lf-list'."
  :type 'boolean
  :group 'emacsw32-eol
  :set (lambda (symbol value)
         (set-default symbol value)
         (if value
             (add-hook 'find-file-not-found-functions
                       'emacsw32-eol-find-file-not-found)
           (remove-hook 'find-file-not-found-functions
            'emacsw32-eol-find-file-not-found))))

(defcustom emacsw32-eol-check-before-save nil
  "If non-nil check end of line is 'unix when saving a file.
This adheres to files matching `emacsw32-eol-file-name-lf-list'."
  :type 'boolean
  :group 'emacsw32-eol
  :set (lambda (symbol value)
         (set-default symbol value)
         (if value
             (add-hook 'before-save-hook
                       'emacsw32-eol-before-save)
           (remove-hook 'before-save-hook
                        'emacsw32-eol-before-save))))

(provide 'emacsw32-eol)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; emacsw32-eol.el ends here
