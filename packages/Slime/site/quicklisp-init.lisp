;; This file is part of Rho installation
;; (C) 2010 g/christensen

;; Mostly borrowed from the original ql load file

(defpackage #:ql-setup
  (:use #:cl)
  (:export #:*quicklisp-home*
           #:qmerge
           #:qenough))

(in-package #:ql-setup)

(unless *load-truename*
  (error "This file must be LOADed to set up quicklisp."))

(defvar *quicklisp-home*
  (merge-pathnames (make-pathname :directory (list :relative ".quicklisp"))
                   (user-homedir-pathname)))

(defvar quicklisp-deployed-p (asdf-config::directory-exists-p *quicklisp-home*))

(defun qmerge (pathname)
  (merge-pathnames pathname *quicklisp-home*))

(defun qenough (pathname)
  (enough-namestring pathname *quicklisp-home*))

(defun file-date< (file1 file2)
  (and (probe-file file1)
       (probe-file file2)
       (< (file-write-date file1)
          (file-write-date file2))))

(defvar *required-asdf-version* "2.009")

(defun ensure-asdf-loaded ()
  (let* ((source (concatenate 'string rho:*rho-dir*
                   "/site/lisp/asdf/asdf.lisp")))
    (labels ((asdf-symbol (name)
               (let ((asdf-package (find-package '#:asdf)))
                 (when asdf-package
                   (find-symbol (string name) asdf-package))))
             (version-satisfies (version)
               (let ((vs-fun (asdf-symbol '#:version-satisfies))
                     (vfun (asdf-symbol '#:asdf-version)))
                 (when (and vs-fun vfun
                            (fboundp vs-fun)
                            (fboundp vfun))
                   (funcall vs-fun (funcall vfun) version)))))
      (block nil
        (macrolet ((try (&body asdf-loading-forms)
                     `(progn
                        (handler-bind ((warning #'muffle-warning))
                          (ignore-errors
                            ,@asdf-loading-forms))
                        (when (version-satisfies *required-asdf-version*)
                          (return t)))))
          (try)
          (try (load source :verbose nil))
          (error "Could not load ASDF ~S or newer" *required-asdf-version*))))))

(ensure-asdf-loaded)

(asdf-config:asdf-setup :init-repositories nil)

(let ((*compile-print* nil)
      (*compile-verbose* nil)
      (*load-verbose* nil)
      (*load-print* nil))
  (asdf:oos 'asdf:load-op "quicklisp" :verbose nil))

(quicklisp:setup)

(when (not quicklisp-deployed-p)
  (push (list *quicklisp-home* asdf-config::*fasl-cache*) asdf-config::*source-to-target-mappings*)

  (asdf:enable-asdf-binary-locations-compatibility :centralize-lisp-binaries nil :map-all-source-files t 
												   :source-to-target-mappings asdf-config::*source-to-target-mappings*))
