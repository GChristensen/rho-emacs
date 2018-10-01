;; package:ABCL ; please do not remove or edit these comments
;; Slime setup ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setf slime-lisp-implementations 
     (append slime-lisp-implementations
       `((abcl (,(concat +rho-dir+ "/batch/run-abcl"))
          :coding-system utf-8-unix))))

(defun rho-run-abcl ()
  (interactive)
  (run-slime-repl 'abcl))

(easy-menu-add-item nil '("((" "Launch") ["ABCL" rho-run-abcl t])

(create-default-get-version 'abcl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Slime setup ;;

