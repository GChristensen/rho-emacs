;; This file is part of Rho installation
;; (C) 2010 g/christensen

;; Common code for ASDF and general initialization 

(defpackage :asdf-config 
  (:use :common-lisp) 
  (:export :get-env-var
           :asdf-setup))

#-sbcl (require 'asdf)

(defun asdf-config:get-env-var (var)
  #+sbcl (sb-ext:posix-getenv var)
  #+ecl (si:getenv var)
  #-(or sbcl ecl) (getenv var))  

(defvar asdf-config::*source-to-target-mappings* nil)
(defvar asdf-config::*fasl-cache* (concatenate 'string (namestring (user-homedir-pathname)) 
                                       "/.fasl-cache/" 
                                       (asdf::implementation-identifier) "/"))

;; main initialization function
(defun asdf-config:asdf-setup (&key init-asdf-install (init-repositories t))
  (let (; root directories
        (rho-dir (asdf-config:get-env-var "RHO_DIR"))
        (home-dir (namestring (user-homedir-pathname)))
        (*load-verbose* nil)
        (*load-print* nil))

    (load (concatenate 'string rho-dir "/site/tiny-fad.lisp"))

    (let* (;; base repositories
           (global-repository (concatenate 'string rho-dir "/site/lisp/repository/"))
           (home-repository (concatenate 'string home-dir "/lisp/repository/"))
           (dev-repository (concatenate 'string home-dir "/lisp/"))
           (lispx-dir (concatenate 'string home-dir "/.lispx/"))
           ;; quicklisp repository
           (quicklisp-home (concatenate 'string home-dir "/.quicklisp/")) 
           (quicklisp-repository (concatenate 'string home-dir "/.quicklisp/dists/quicklisp/software/"))
           (quicklisp-deployed-p (when (asdf-config::directory-exists-p quicklisp-repository) t))
           ;; Weblocks examples are hidden in the Weblocks directory structure and are not accessible
           ;; by default
           #+ccl (weblocks-examples (concatenate 'string home-repository "weblocks/examples/"))
           #+ccl (weblocks-examples (when (asdf-config::directory-exists-p weblocks-examples) weblocks-examples))
           ;; fasl cache path
           (fasl-cache asdf-config::*fasl-cache*))

      (ensure-directories-exist home-repository)
      (ensure-directories-exist dev-repository)

      ;; scan for ASDF systems and add corresponding directories to asdf:*central-registry*
      ;; lispx applications are treated specially
      (labels ((init-repository (base-dir)
           ;; treat all subdirs in common repository containing *.asd files as ASDF-system hosting directories
           ;; presence of `.noindex' file forbids further scanning
           (when (not (asdf-config::file-exists-p (concatenate 'string base-dir ".noindex")))
             (dolist (dir-candidate (directory (concatenate 'string base-dir "*" #-abcl "/") 
                                               #+ccl :directories #+ccl t))
               (flet ((current-child (p) (merge-pathnames p dir-candidate)))
                 (if (asdf-config::file-exists-p (current-child (make-pathname :name "application" :type "meta")))
                     ;; found lispx application
                     (let ((source-dir (current-child (make-pathname :directory '(:relative "source"))))
                           (systems-dir (current-child (make-pathname :directory '(:relative "systems")))))
                       ;; treat application `source' and `systems' subdirs as yet another ASDF-repositories
                       (init-repository (namestring systems-dir))
                       (init-repository (namestring source-dir))
                       (push source-dir asdf:*central-registry*)))
                     (let ((asd-candidate (merge-pathnames "*.asd" dir-candidate)))
                       (when (directory asd-candidate)
                       (push dir-candidate asdf:*central-registry*))))))))

       (when init-repositories
        (init-repository global-repository)

		(push (pathname quicklisp-home) asdf:*central-registry*)
        (when quicklisp-deployed-p 
         (init-repository quicklisp-repository))

        (init-repository home-repository)

        #+ccl
        (when weblocks-examples 
         (init-repository weblocks-examples))

        (init-repository dev-repository)))
     
       (setf asdf-config::*source-to-target-mappings*
             `((,global-repository ,fasl-cache)
               (,home-repository ,fasl-cache)
               (,dev-repository ,fasl-cache)
               #+sbcl (,(concatenate 'string rho-dir "/bin/sbcl/") nil)))

       (when (asdf-config::directory-exists-p quicklisp-home)
         (push (list quicklisp-home fasl-cache) asdf-config::*source-to-target-mappings*))

       ;; redirect lispx-proxy cache
       (when (asdf-config::directory-exists-p lispx-dir)
         (push (list lispx-dir (concatenate 'string fasl-cache ".lispx/")) 
               asdf-config::*source-to-target-mappings*))

       #+ccl
       (when weblocks-examples
         (push (list weblocks-examples fasl-cache) asdf-config::*source-to-target-mappings*))

       (asdf:enable-asdf-binary-locations-compatibility :centralize-lisp-binaries nil :map-all-source-files t 
														:source-to-target-mappings asdf-config::*source-to-target-mappings*)

       ;; rho package initialization
       (load (concatenate 'string rho-dir "/site/rho-init.lisp"))

       ;; initialize package management systems
       (when (and init-asdf-install (not (string= (asdf-config:get-env-var "NO_ASDF_INSTALL") "t")))
         ;; ASDF-INSTALL
         ;#-(or ecl abcl) (asdf:oos 'asdf:load-op :asdf-install :verbose nil)
         ;#-(or ecl abcl) (load (concatenate 'string rho-dir "/site/asdf-install-init.lisp"))
         
         ;; quicklisp
         (load (concatenate 'string rho-dir "/site/quicklisp-init.lisp"))))))
