(let ((lisp-dir (expand-file-name (concat +rho-dir+ "/bin/emacs/EmacsW32/lisp/"))))
  (unless (file-accessible-directory-p lisp-dir)
    (lwarn "Can't find %s" lisp-dir)
    (sit-for 10))
  (when (file-accessible-directory-p lisp-dir)
    (message "Adding %s to load-path" lisp-dir)
    (add-to-list 'load-path lisp-dir))
  (require 'emacsw32 nil t)
  (unless (featurep 'emacsw32)
    (lwarn '(emacsw32) :error "Could not find emacsw32.el")))