;;; mkziplst.el --- Creates mkZip.lst
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2004
;; Version: 0.85
;; Last-Updated: Mon Jan 15 03:08:49 2007 (3600 +0100)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; For internal use creating installers.
;;
;; This file reads the list of files included by Inno from
;; EmacsW32.iss and writes the list of files to mkZip.lst in a
;; format that can be used by mkZip.cmd.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; History:
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:

(defun emacsw32-lstdir-from-load-file ()
  (let ((load-dir (file-name-directory (if load-file-name
                                           load-file-name
                                         buffer-file-name))))
    (file-name-as-directory (expand-file-name ".." load-dir))))

(defun mkziplst-find-in-iss ()
  (let (
        ;;(iss-file (expand-file-name (concat +exec-directory+ "../../EmacsW32/EmacsW32.iss")))
        (iss-file (expand-file-name "EmacsW32.iss" (emacsw32-lstdir-from-load-file)))
	;;(lst-file (expand-file-name (concat +exec-directory+ "../../EmacsW32/mkZip.lst")))
	(lst-file (expand-file-name "mkZip.lst" (emacsw32-lstdir-from-load-file)))
	files)
    (unless (file-exists-p iss-file)
      (error "Can't find %s" iss-file))
    (when (file-exists-p lst-file)
      (error "Output file must not exist: %s" lst-file))
    (save-excursion
      (save-window-excursion
	(find-file iss-file)
	(widen)
	(goto-char (point-min))
	(while (re-search-forward "^\\s-*Source:\\s-*\"\\(.*?\\)\"" nil t)
	  (let ((file (match-string 1)))
	    (unless (member file files)
	      (setq files (cons (match-string 1) files)))))))

    ;; Write the zip file list
    (setq files (sort files 'string<))
    (find-file lst-file)
    (while files
      (insert "EmacsW32/" (car files) "\n")
      (setq files (cdr files)))
    (save-buffer)
    (message "Created %s" (buffer-file-name))
    (kill-buffer (current-buffer))
    ))

(mkziplst-find-in-iss)

(provide 'mkziplst)

;;; mkziplst.el ends here
