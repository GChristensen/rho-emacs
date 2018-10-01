;; This file is part of Rho installation
;; (C) 2010 g/christensen

;; Clozure Common Lisp initialization script

;; Override user-homedir-pathname to get user home directory path
;; from environment variable (disabled in CLL by unknown reason)
(let ((ccl::*warn-if-redefine-kernel* nil))
  (defun ccl::user-homedir-pathname (&optional host)
    "Return the home directory of the user as a pathname."
    (declare (ignore host))
    (let* ((native
	    (ignore-errors
	      (truename
	       (ccl::native-to-directory-pathname 
		(or (let ((home-dir (getenv "HOME_UTF8")))
		      (when home-dir
			(decode-string-from-octets 
			 (encode-string-to-octets home-dir) 
			 :external-format :UTF-8)))
		    (ccl::get-user-home-dir (ccl::getuid))))))))
      (if (and native (eq :absolute (car (pathname-directory native))))
	  native
	(make-pathname :directory '(:absolute) :defaults nil)))))

(load (concatenate 'string (getenv "RHO_DIR") "/site/asdf-init.lisp"))
(asdf-config:asdf-setup :init-asdf-install t)

