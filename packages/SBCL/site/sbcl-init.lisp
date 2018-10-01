;; This file is part of Rho installation

;; SBCL initialization script

(require :asdf)

;; Override `asdf::contrib-sysdef-search' to disable SBCL buit-in ASDF-INSTALL.
;; Probably more subtle hooking is needed.
(defun asdf::contrib-sysdef-search (system)
  (let ((home (sb-ext:posix-getenv "SBCL_HOME")))
    (when (and (not (string= system "asdf-install")) home (not (string= home "")))
      (let* ((name (asdf::coerce-name system))
             (home (truename home))
             (contrib (merge-pathnames
                       (make-pathname :directory `(:relative ,name)
                                      :name name
                                      :type "asd"
                                      :case :local
                                      :version :newest)
                       home)))
        (probe-file contrib)))))


(load (concatenate 'string (sb-ext:posix-getenv "RHO_DIR")
                   "/site/asdf-init.lisp"))

(asdf-config:asdf-setup :init-asdf-install t)
