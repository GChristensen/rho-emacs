;; This file is part of Rho installation
;; (C) 2010 g/christensen

;; ASDF-INSTALL initialization script

(in-package :asdf-install)

(asdf:oos 'asdf:load-op :external-program :verbose nil)

; Use bsdtar to extract packages
(defvar *path-sentinel*
  #+(or ccl abcl) "\"~a\""
  #+(or sbcl clisp) "~a")

(defun custom-extractor (to-dir tarball)
  (let* ((tarball (namestring (truename tarball)))
		 (target-dir (namestring (truename to-dir)))
         (trail-char (char target-dir (1- (length target-dir))))
	     (target-dir (if (or (eql trail-char #\\) (eql trail-char #\/))
                         (subseq target-dir 0 (1- (length target-dir)))
                         target-dir))
		 (tar-listing (concatenate 'string tarball ".list"))
         (script (concatenate 'string (get-env-var "RHO_DIR") "/bin/utils/run-tar.vbs")))
	(external-program:run "cscript"
						  (list (format nil *path-sentinel* script)
                                (format nil *path-sentinel* tarball) 
                                (format nil *path-sentinel* target-dir) 
                                (format nil *path-sentinel* tar-listing)))
	(prog1 (with-open-file (strm tar-listing :direction :input)
			 (subseq (read-line strm) 2))
	  (delete-file tar-listing))))

  
(setf *tar-extractors* (list #'custom-extractor))

;; Disable GPG signature verification
(defparameter asdf-install-customize::*verify-gpg-signatures* nil)

;; Install into private common repository without prompt
(setf *preferred-location* "Personal installation")

;; Disable any shell commands executed by ASDF-INSTALL
(defun shell-command (s)
 (declare (ignore s)))

;; Override default ASDF-INSTALL repository locations (same for all lisps)
(setf *private-asdf-install-dirs* 
	  (let ((site-dir (concatenate 'string 
								   (namestring (user-homedir-pathname))
								   "/lisp/")))
		(directorify site-dir)))

(setf *locations*
  `((,(merge-pathnames (make-pathname :directory '(:relative "repository"))
                       asdf-install::*private-asdf-install-dirs*)
     ,(merge-pathnames (make-pathname :directory '(:relative "repository"))
                       asdf-install::*private-asdf-install-dirs*)
     "Personal installation")))

(setf *temporary-directory* (directorify (get-env-var "TEMP")))