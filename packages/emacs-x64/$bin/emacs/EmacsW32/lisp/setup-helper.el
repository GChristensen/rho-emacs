;;; setup-helper.el --- Functions for installation and setup of Emacs
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2005-09-18
;; Version: 1.84
;; Last-Updated: Thu Jan 18 03:02:55 2007 (3600 +0100)
;; Keywords: installation setup

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This is a collection of functions maybe useful when installing or
;; setting up emacs.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; History:
;;
;; BETA VERSION!!!!! NOT READY!!!


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

(defvar setup-helper-recursive-quit nil
  "Help variable to get out of recursive calls to replace functions.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Replace sexp

;;(defconst mytimes 0)

(defvar setup-helper-overlay nil)

(defun setup-helper-mk-overlay (start end)
  (when setup-helper-overlay
    (delete-overlay setup-helper-overlay))
  (setq setup-helper-overlay (make-overlay 0 0))
  ;; Use the same marking as query-replace, but add strings before and after
  (overlay-put setup-helper-overlay 'before-string "<<<")
  (overlay-put setup-helper-overlay 'after-string  ">>>")
  (overlay-put setup-helper-overlay 'face
	       (if (facep 'query-replace)
		   'query-replace 'region))
	       ;;(cons 'background-color "yellow"))
  (move-overlay setup-helper-overlay start end)
  )
;;FIX: use customize-set-variable and customize-save-variable







(defun setup-helper-find-replace-lisp (old-lisp
				      &optional
				      old-is-regexp
				      replacement
				      query
				      dont-recurse
				      max-point dd)
  "Find and optionally replace a string representing a lisp object.

Searches for OLD-LISP.  Only searches the next lisp object in the
current buffer.  To search the whole of the buffer use
`setup-helper-find-replace-lisp-whole'.

OLD-LISP is considered to be a lisp object and matched with `equal'
unless OLD-IS-REGEXP is non-nil in which case OLD-LISP must be a
string which is used as a regular expression to match the print
representation of whole lisp objects.

REPLAMENT must be nil, a string or 'comment-out. When REPLACEMENT is a
string OLD-LISP are replaced with REPLACEMENT.  If REPLACEMENT is
'comment-out OLD-LISP is commented out.  In this case DONT-RECURSE
must be non-nil. (This restriction is there to avoid problems with
syntax or semantics of the changed lisp code.)

Unless DONT-RECURSE is non-nil the search recurses into inner lisp
objects.

When QUERY is non-nil it must be a string and the user is asked to
accept or deny each change. In this case the prompt is constructed
from the string QUERY where ${new} in this string is replaced with
with REPLACEMENT.

MAX-POINT is used internally to bound the search.  DD is used for
debugging.

If REPLACEMENT is non-nil return t if OLD-LISP is found.  If
REPLACEMENT is nil return t if there are more lisp objects to read on
current level of recursion.  Otherwise return nil.

Set position after read lisp object.
"
  (interactive)
;;   (message "RL old=%s regexp=%s r=%s q=%s dr=%s %s %s"
;; 	   old-lisp old-is-regexp replacement query dont-recurse max-point dd)
  ;;(setq mytimes (+ mytimes 1))

  ;;;; Check params
  (when old-is-regexp
    (unless (stringp old-lisp)
      (error "OLD-LISP must be a string if OLD-IS-REGEXP is non-nil"))
    (unless (equal (substring old-lisp 0 2) "\\`")
      (error "When OLD-LISP is a regexp it must start with \\\\'"))
    (unless (equal (substring old-lisp -2) "\\'")
      (error "When OLD-LISP is a regexp it must end with \\\\'")) )
  (when (and (eq replacement 'comment-out) (not dont-recurse))
    (error "DONT-RECURSE must be t if REPLACEMENT is 'comment-out"))
  (unless dd (setq dd 0))

  (unless setup-helper-recursive-quit
    (while (forward-comment 1))
    (let* ((dbg nil)
	   (start (point-marker))
	   (eo-level)
	   (read-sexp) ;; read lisp object
	   (read-sexp-str) ;; read lisp object as string
	   (buffer-sexp) ;; the actually read text
	   (new-max)
	   (hit)
	   (replaced)
	   (comment-out (eq replacement 'comment-out)))
      (when dbg (message "old-lisp=%s" (prin1-to-string old-lisp)))
      (if (and max-point (> (point) max-point))
	  (progn (setq read-sexp t) (setq eo-level t))
	(condition-case nil
	    (progn (setq read-sexp (read (current-buffer)))
		   (setq buffer-sexp (buffer-substring start (point)))
		   (setq new-max (point)))
	  (error (setq read-sexp t) (setq eo-level t))))
      (unless (or eo-level (not read-sexp))
	(setq read-sexp-str (prin1-to-string read-sexp))
	(setq hit (if old-is-regexp
		      (progn
;; 			(message "matching old-lisp=%s buffer-sexp=%s\n=== dont-recurse=%s"
;; 				 old-lisp buffer-sexp dont-recurse)
			(string-match old-lisp buffer-sexp)
			)
		    (equal read-sexp old-lisp)))
	;;(when hit (message "hit read-sexp=%s" read-sexp))
	(save-excursion
	  (if hit
	      (when replacement
		(if comment-out
		    (let ((end (point-marker))
			  (do-it t))
		      ;;(message "replace-lisp.comment-out.query=%s" query)
		      (if query (let ((prompt query))
				  (setup-helper-mk-overlay start end)
				  (condition-case nil
				      (setq do-it (y-or-n-p prompt))
				    (quit
				     (setq do-it nil)
				     (setq setup-helper-recursive-quit t)
				     (delete-overlay setup-helper-overlay)))
				  (delete-overlay setup-helper-overlay)))
		      (if do-it
			  (save-excursion
			    ;;(goto-char (+ end 2))
			    (insert "\n")
			    (goto-char start)
			    (insert
			     (concat "\n**** Commented out by setup-helper-find-replace-lisp ("
				     (current-time-string) ")\n"))
			    (comment-region start end)
			    (message "Commented out.")
			    )
			(message "Skipped."))
		      )
		  (let* ((end (point-marker))
			 (repl-in-repl
			  (save-match-data
			    (if old-is-regexp nil
			      (let ((pos 1)
				    (lst nil))
				(while (setq pos (string-match "\\\\&" replacement pos))
				  (push pos lst)
				  (setq pos (+ pos 1)))
				lst))))
			 (old-text)
			 (new-text
			  (if old-is-regexp
			      (progn (replace-match replacement t nil buffer-sexp))
			    (if repl-in-repl
				(mapc (lambda (pos)
					(setq new-text
					      (concat (substring replacement 0 (- pos 1))
						      old-text
						      (substring replacement (+ pos +2)))))
				      repl-in-repl)
			      replacement)))
			 (do-it t))
		    ;;(message "replace-lisp.not comment-out.query=%s" query)
		    (if query
			(save-match-data
			  (let ((prompt (replace-regexp-in-string
					 "${new}" new-text query)))
			    (setup-helper-mk-overlay start end)
			    ;;(message "replacement=%s" replacement)
			    ;;(message "buffer-sexp=%s" buffer-sexp)
			    ;;(message "read-sexp-str=%s" read-sexp-str)
			    ;;(message "old-is-regexp=%s" old-is-regexp)
			    ;;(message "repl-in-repl=%s" repl-in-repl)
			    ;;(message "query=%s" query)
			    ;;(message "new-text=%s" new-text)
			    ;;(message "prompt=%s" prompt)
			    (condition-case nil
				(setq do-it (y-or-n-p prompt))
			      (quit
			       (setq do-it nil)
			       (setq setup-helper-recursive-quit t)
			       (delete-overlay setup-helper-overlay)))
			    (delete-overlay setup-helper-overlay)))
		      )
		    (if (not do-it)
			(message "Skipped.")
		      (when dbg (message "YES %s %s %s=%s" dd repl-in-repl start read-sexp-str))
		      (save-excursion
			;;(when nil
			(let ((cmnt-text (buffer-substring
					  (save-excursion
					    (goto-char start)
					    (beginning-of-line)
					    (point))
					  (save-excursion
					    (goto-char end)
					    (end-of-line)
					    (point)))))
			  (setq old-text (delete-and-extract-region start end))
			  (unless (equal old-text buffer-sexp)
			    (error "Old-text <> buffer-sexp"))
			  (save-excursion
			    (beginning-of-line)
			    (let ((cstart (point)))
			      (insert
			       (concat "\n**** Commented out by setup-helper-find-replace-lisp ("
				       (current-time-string) ")\n"
				       cmnt-text
				       "\n**** and replaced with:\n"))
			      (comment-region cstart (point))))
			  (goto-char start)
			  (insert new-text)))
		      (message "Replaced.")
		      (setq replaced t)
		      )))
		(sit-for 2)
		)
	    (when (not dont-recurse)
	      ;;(message "NO %s %s %s" dd (listp read-sexp) read-sexp)
	      (when (and read-sexp
			 (listp read-sexp))
		;; goto after opening parenthesis
		(goto-char start) (forward-char)
		(while (< (point) new-max)
		  (when
		      (setup-helper-find-replace-lisp old-lisp old-is-regexp replacement
						      query dont-recurse new-max (+ dd 1))
		    (setq replaced t)))
		;;(setq eo-level t)
		)))))
      (if replacement (not eo-level) hit)
      hit
      (if replacement replaced hit)
      )))


(defun setup-helper-find-replace-lisp-whole (old-lisp
					    &optional
					    old-is-regexp replacement
					    query
					    dont-recurse
					    max-point dd)
  "See `setup-helper-find-replace-lisp'."
  (interactive)
  (setq setup-helper-recursive-quit nil)
  (save-excursion
    (save-window-excursion
      (save-match-data
	(goto-char (point-min))
	(let ((hit))
	  (while (not (eobp))
	    ;;(message "whole while")
	    (when (setup-helper-find-replace-lisp
		   old-lisp old-is-regexp replacement query dont-recurse max-point)
	      (setq hit t)))
	  hit)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Add sexp

(defun setup-helper-add-sexp-if-not-found (file sexpstr &optional comment recurse append)
  "Add sexp to a file if it is not found in the file.
SEXPSTR should be given as a string.

If COMMENT is non-nil it is inserted before SEXPSTR and commented out.

If RECURSE is non-nil look inside the lisp object if it is a list and
does not match.

When APPEND is non-nil do the insertion at the end, otherwise at the
beginning.

Return t when changing, nil otherwise."
  ;;(message "add-sexp-inf %s %s %s %s %s" file sexpstr comment recurse append)
  ;;(let ((sexp (read sexpstr)))
  (let ((sexp (with-temp-buffer
                (insert sexpstr)
                (goto-char (point-min))
                (let ((sx (read (current-buffer))))
                  (forward-comment 1)
                  (unless (eobp)
                    (error "More than one sexp in arg sexpstr"))
                  sx))))
    (unless (listp sexp)
      (error "Only a full sexp can be added"))
    (save-excursion
      (find-file file)
      (if (setup-helper-find-replace-lisp-whole sexp nil nil nil (not recurse))
	  (progn
	    (message "Sexp is already in %s" file)
	    nil)
	(message "Adding sexp to %s" file)
	(if append
	    (goto-char (point-max))
	  (goto-char (point-min)))
	(when append (unless (bobp) (insert "\n\n")))
	(when comment
	  (let ((start (point-marker)))
	    (insert comment "\n")
	    (comment-region start (point))))
	(let ((start (point-marker)))
	  (insert sexpstr)
	  (indent-region start (point) nil))
	(unless append (unless (eobp) (insert "\n\n\n")))
	(save-buffer)
	t))))

;; (defun comment-out-cua ()
;;   (interactive)
;;   (let* ((sexplist (list
;; 		    '(require (quote cua))
;; 		    '(require (quote cua) nil t)
;; 		    '(setq CUA-mode t)
;; 		    '(setq CUA-mode nil)
;; 		    '(set (quote CUA-mode) t)
;; 		    '(set (quote CUA-mode) nil)
;; 		    )))
;;     (setup-helper-comment-all-matching-sexp sexplist)
;;     ))






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Old modules

(defun setup-helper-fix-libdbl (library)
  (interactive "sLibrary: ")
  (let* ((file (locate-library library)) (dir) (ver) (verstr)
	 (files (list))
	 (path (mapcar (lambda (dir) (file-name-as-directory (expand-file-name dir)))
		       load-path))
	 )
    (while file
      (setq dir (file-name-directory file))
      (delete dir path)
      (setq ver 0)
      (with-temp-buffer
	(insert-file-contents file)
	(goto-char (point-min))
	(re-search-forward ";;\\s +Version:\\s +\\(\\S-+\\)")
	(when (match-beginning 1)
	  (message "%s (%s)" (match-string 0) (match-string 1))
	  (setq verstr (match-string 1))
	  (setq ver (string-to-number verstr))
	  )
	)
      (push (vector file ver verstr) files)
      ;;(setq files (append files (list (cons file (vector ver verstr)))))
      (message "files=%s dir=%s" files dir)
      (setq file (locate-library library nil path))
      )
    (when (> (length files) 1)
      (let* ((ol (reverse files))
	     (first (car ol))
	     (next)
	     )
	(message "ol=%s" ol)
	(message "first=%s" first)
	(setq ol (cdr ol))
	(while ol
	  (setq next (car ol))
	  (when (< (elt first 1) (elt next 1))
	    (message "next=%s" next)
	    (if (y-or-n-p
		 (concat "Version " (elt next 2) " of module " library
			 " is available on your computer, but it is currently hidden by "
			 " the file " (elt first 0) " which contains version " (elt first 2)
			 ". Do you want to rename this file so that"
			 " the newer version can be used?"))
		(message "rename")
	      (message "Ok, keeping old version"))
	    )
	  (setq ol (cdr ol))
	  )
	))
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dirs and files

;; I do not know if this is the case for all Emacs installations, but
;; at least in my 21.3 MS Windows installation there are two site-lisp
;; dirs in load-path.

(defun setup-helper-site-lisp1()
  "Path to site-lisp outer dir"
  (expand-file-name (concat +exec-directory+ "../../site-lisp/")))
(defun setup-helper-site-lisp2()
  "Path to site-lisp inner dir"
  (expand-file-name (concat +exec-directory+ "../site-lisp/")))

(defun setup-helper-dot-emacs ()
  "Get the full path name to .emacs"
  (expand-file-name "~/.emacs"))

(defun setup-helper-default-el-file-name1()
  "Get the full path name to use for an outer default.el.
*Note: This is useful only if there is no library \"default\" in
load-path and you want to create one.  To find the current use
\(locate-library \"default\"\)."
  (expand-file-name (concat +exec-directory+ "../../site-lisp/default.el")))
(defun setup-helper-default-el-file-name2()
  "Get the full path name to use for an inner default.el.
Please see the note in `setup-helper-default-el-file-name1'."
  (expand-file-name (concat +exec-directory+ "../site-lisp/default.el")))

(defun setup-helper-site-start-el-file-name1()
  "Get the full path name to use for an outer site-start.el.
*Note: This is useful only if there is no library `site-run-file' in
load-path and you want to create one.  To find the current use
\(locate-library \(or site-run-file \"site-start\"\)\)."
  (expand-file-name (concat +exec-directory+ "../../site-lisp/site-start.el")))
(defun setup-helper-site-start-el-file-name2()
  "Get the full path name to use for an inner site-start.el
Please see the note in `setup-helper-site-start-el-file-name1'."
  (expand-file-name (concat +exec-directory+ "../site-lisp/site-start.el")))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customize

(defun setup-helper-custom-set-to-save (symbol value)
  "Set SYMBOL to VALUE and add to customize.
Both the current value and the value to save is set, but
`custom-save-all' must be called to save customization."
  (customize-set-variable symbol value)
  (customize-set-value symbol value)
  (customize-mark-to-save symbol))

;; (defun setup-helper-set-as-custom (symbol)
;;   "Set SYMBOL the same way `custom-set-variables' should do."
;;   (let* ((value (car (get symbol 'saved-value)))
;; 	 (now (get symbol 'force-value))
;; 	 (requests (get symbol 'custom-requests))
;; 	 (comment (get symbol 'comment))
;; 	 (cuslist (list value symbol)))
;;     (when (or now requests comment) (setq cuslist (cons now cuslist)))
;;     (when (or requests comment) (setq cuslist (cons requests cuslist)))
;;     (when comment (setq cuslist (cons comment cuslist)))
;;     (setq cuslist (reverse cuslist))
;;     (message "AAAA, cuslist=%s" cuslist)
;;     (custom-set-variables cuslist)
;;     (message "BBB")
;;     ;;(message "%s" cuslist)
;;     ))

(defun setup-helper-erase-customization (symbol)
  "Erase customization for SYMBOL."
  (put symbol 'saved-value nil)
  (put symbol 'custom-requests nil)
  (put symbol 'force-value nil)
  (put symbol 'comment nil)
  (put symbol 'saved-variable-comment nil)
  (funcall (or (get symbol 'custom-set) 'set-default)
	   symbol
	   (eval (car (get symbol 'standard-value)))))

;; (defun setup-helper-add-custom (symbol value &optional now requests comment)
;;   "Sets those properties of SYMBOL that are saved in `custom-file'.
;; For an explanation of VALUE, NOW, REQUESTS and COMMENT see
;; `custom-set-variables'.  If COMMENT is nil a comment is added by this
;; function.

;; The current value of SYMBOL is not set. To set the value use
;; `setup-helper-set-as-custom'.

;; If you call `custom-save-all' SYMBOL will be saved to the
;; `custom-set-variables' entry."
;;   ;;(message "add-custom %s %s %s %s %s" symbol value now requests comment)
;;   ;; FIX-ME: how to construct the value? Needs another quoting now.
;;   (put symbol 'saved-value (list value))
;;   (put symbol 'custom-requests (when requests (list requests)))
;;   (put symbol 'force-value now)
;;   (unless comment (setq comment "Added by setup-helper"))
;;   (put symbol 'saved-variable-comment comment))

;; Use these lines to test:
;;(setup-helper-add-custom 'aaa "test" nil)
;;(setup-helper-set-as-custom 'aaa)
;;(defvar aaa nil)
;;(setup-helper-set-as-custom 'aaa)

;; This is riped from cus-edit.el customize-option, Emacs 21.3.1
(defun setup-helper-customizeable (symbol)
  "Return t if SYMBOL can be customized, nil otherwise."
  (interactive (custom-variable-prompt))
  ;; If we don't have SYMBOL's real definition loaded,
  ;; try to load it.
  (unless (get symbol 'custom-type)
    (let ((loaddefs-file (locate-library "loaddefs.el" t))
	  file)
      ;; See if it is autoloaded from some library.
      (when loaddefs-file
	(with-temp-buffer
	  (insert-file-contents loaddefs-file)
	  (when (re-search-forward (concat "^(defvar " (symbol-name symbol))
				   nil t)
	    (search-backward "\n;;; Generated autoloads from ")
	    (goto-char (match-end 0))
	    (setq file (buffer-substring (point)
					 (progn (end-of-line) (point)))))))
      ;; If it is, load that library.
      (when file
	(when (string-match "\\.el\\'" file)
	  (setq file (substring file 0 (match-beginning 0))))
	(load file))))
  (if (get symbol 'custom-type) t nil))


(defun setup-helper-customize-list (defgroup-var defcustom-list)
  (mapcar (lambda (var)
	    (custom-add-to-group defgroup-var var 'custom-variable))
	  defcustom-list)
  (customize-group defgroup-var))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Add lines (comments)

;; I do not like it - to unsafe, just saves the code here!
;; (defun setup-helper-add-lines-if-not-found (file lines append)
;;   "Adds lines to a file if it is not found in the file."
;;   (let ((appendre   (concat "^"  (replace-regexp-in-string "\(" "\(" lines) "$"))
;; 	(file-buffer (find-file-noselect file)))
;;     (save-excursion
;;       (set-buffer file-buffer)
;;       (let ((nonempty-match
;; 	     (if (string-match appendre (buffer-string)) t nil)))
;; 	(if nonempty-match
;; 	    (message "Lines were already in file...")
;; 	  (message "Inserting lines...")
;; 	  (if append
;; 	      (goto-char (point-max))
;; 	    (goto-char (point-min)))
;; 	  (let ((appendline
;; 		 (concat (when append "\n\n\n")
;; 			 ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n"
;; 			 ";;;; Following line(s) inserted by setup-helper.el:\n"
;; 			 lines
;; 			 (unless append "\n\n\n"))))
;; 	    (insert appendline)
;; 	    (save-buffer) ) )))))


;; (defun setup-helper-repl-all-regexp (srchexp replstr)
;;   "Replace matches of SRCHEXP with REPLSTR in current buffer."
;;   (save-excursion
;;     (goto-char (point-min))
;;     (replace-regexp srchexp replstr)))

(defun setup-helper-q (str)
  "Quote string STR - for better syntax hilighting..."
  (let ((q "\""))
    (concat q str q)))

(provide 'setup-helper)

;;; setup-helper.el ends here
