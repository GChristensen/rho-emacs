;; This file is part of Rho installation
;; (C) 2010 g/christensen

;; Call non elisp code

(defconst +left-delim+ "@>")
(defconst +right-delim+ "<@")

(defun inject-lisp-impl ()
  (eval (read (format "#'impl-%s-inject-lisp" (symbol-name *script-lang*)))))

(defun assemble-cmdline-impl ()
  (eval (read (format "#'impl-%s-assemble-cmdline" 
					  (symbol-name *script-lang*)))))

(defun lisp-escape-backslashes (str)
  (replace-regexp-in-string (regexp-quote "\\") 
							(regexp-quote "\\\\") str))

(defun lisp-escape-quotes (str)
  (replace-regexp-in-string (regexp-quote "\"" )
							(regexp-quote "\\\"") str))

(defun lisp-strip-values (str)
  (replace-regexp-in-string 
   (format "%s\\(.*?\\)%s" +left-delim+ +right-delim+)
   '(lambda (str) 	  
	  (lisp-escape-quotes
	   (lisp-escape-backslashes (match-string 1 str))))
	  str nil t 0))

(defun alien-eval (form code lang)
  (let ((script-name (concat (getenv "TEMP") "/alien-bind-el." 
							 (symbol-name (gensym))))
		(*script-lang* lang))
										  
    (with-temp-file script-name 
	  (insert
	   (apply (inject-lisp-impl) (list form code))))
    (prog1 (read (lisp-strip-values
				  (shell-command-to-string 
				   (apply (assemble-cmdline-impl) (list script-name)))))
	  (delete-file script-name))))

(defun decorate-var (var)
  (concat +left-delim+ 
		  (replace-regexp-in-string "-" "_" (symbol-name var)) 
		  +right-delim+))

(defun decorate-vars (vars)
  (mapcar #'(lambda (var)
			  (list var (decorate-var var)))
		  vars))

(defmacro alien-bind (vars lang code &rest body)
  `(eval `(let ,(alien-eval (decorate-vars ',vars) ,code ,lang)
			,@',body)))

;; ----- VBS specific code ----------------------------------------------------

(defconst +vbs-left-stripper+ (format "%s\" & " +left-delim+))
(defconst +vbs-right-stripper+ (format " & \"%s" +right-delim+))

(defun vbs-escape-quotes (str)
  (replace-regexp-in-string "\"" "\"\"" str))

(defun vbs-strip-vars (str)
  (replace-regexp-in-string 
   +left-delim+ +vbs-left-stripper+
   (replace-regexp-in-string +right-delim+ +vbs-right-stripper+
							 str)))

(defun impl-vbs-inject-lisp (form code)
  (format "%s
          Wscript.StdOut.WriteLine \"%s\"" code
		  (vbs-strip-vars 
		   (vbs-escape-quotes (format "%S" form)))))

(defun impl-vbs-assemble-cmdline (file)
  (concat "cscript //Nologo //E:vbs \"" file "\""))

;; ---------------------------------------------------- VBS specific code -----
