;; package:SBCL ; please do not remove or edit these comments
;; Slime setup ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setf slime-lisp-implementations 
     (append slime-lisp-implementations
       `((sbcl (,(concat +rho-dir+ "/batch/run-sbcl"))
          :coding-system utf-8-unix))))

(defun rho-run-sbcl ()
  (interactive)
  (run-slime-repl 'sbcl))

(easy-menu-add-item nil '("((" "Launch") ["SBCL" rho-run-sbcl t])

(create-default-get-version 'sbcl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Slime setup ;;

