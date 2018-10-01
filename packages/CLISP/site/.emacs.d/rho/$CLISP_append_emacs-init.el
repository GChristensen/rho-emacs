;; package:clisp ; please do not remove or edit these comments
;; Slime setup ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setf slime-lisp-implementations 
     (append slime-lisp-implementations
       `((clisp (,(concat +rho-dir+ "/batch/run-clisp"))
          :coding-system utf-8-unix))))

(defun rho-run-clisp ()
  (interactive)
  (run-slime-repl 'clisp))

(easy-menu-add-item nil '("((" "Launch") ["CLISP" rho-run-clisp t])

(create-default-get-version 'clisp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Slime setup ;;

