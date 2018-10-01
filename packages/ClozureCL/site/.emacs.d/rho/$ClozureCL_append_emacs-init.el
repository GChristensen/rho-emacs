;; package:ClozureCL ; please do not remove or edit these comments
;; Slime setup ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setf slime-lisp-implementations 
     (append slime-lisp-implementations
       `((ccl (,(concat +rho-dir+ "/batch/run-ccl"))
          :coding-system utf-8-unix))))

(defun rho-run-ccl ()
  (interactive)
  (run-slime-repl 'ccl))

(easy-menu-add-item nil '("((" "Launch") ["Clozure CL" rho-run-ccl t])

(create-default-get-version 'clozurecl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Slime setup ;;

