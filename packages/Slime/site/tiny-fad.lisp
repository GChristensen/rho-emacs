;; This file is part of Rho installation
;; (C) 2010 g/christensen

;; excerpts from cl-fad (we need this, because we have not configured 
;; asdf:*central-repository* yet) 

(in-package :asdf-config)

(defun component-present-p (value)
  (and value (not (eql value :unspecific))))

(defun directory-pathname-p (pathspec)
  (and 
    (not (component-present-p (pathname-name pathspec)))
    (not (component-present-p (pathname-type pathspec)))
    pathspec))

(defun pathname-as-directory (pathspec)
  (let ((pathname (pathname pathspec)))
    (when (wild-pathname-p pathname)
      (error "Can't reliably convert wild pathnames."))
    (cond ((not (directory-pathname-p pathspec))
           (make-pathname :directory (append (or (pathname-directory pathname)
                                                 (list :relative))
                                             (list (file-namestring pathname)))
                          :name nil
                          :type nil
                          :defaults pathname))
          (t pathname))))

(defun file-exists-p (pathspec)
  #+(or sbcl openmcl ecl) (probe-file pathspec)
  #+abcl (or (probe-file (pathname-as-directory pathspec))
                             (probe-file pathspec))
  #+:clisp (or (ignore-errors
                 (let ((directory-form (pathname-as-directory pathspec)))
                   (when (ext:probe-directory directory-form)
                     directory-form)))
               (ignore-errors
                 (probe-file (pathname-as-file pathspec)))))

(defun directory-exists-p (pathspec)
  (let ((result (file-exists-p pathspec)))
    (and result
         (directory-pathname-p result)
         result)))
