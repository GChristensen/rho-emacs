;; package:HyperSpec ; please do not remove or edit these comments
;; Slime setup ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar common-lisp-hyperspec-root 
        (concat "file:///" (substitute ?/ ?\\ +rho-dir+) "/docs/HyperSpec/"))


(defun rho-browse-cl-docs ()
 (interactive)
 (w32-shell-execute "open" (replace-regexp-in-string 
                            "/" (regexp-quote "\\")
                             (concat +rho-dir+ "/docs/HyperSpec/Front/index.htm"))))

(easy-menu-add-item nil '("((" "Browse Documentation") ["HyperSpec" rho-browse-cl-docs t])


(create-default-get-version 'hyperspec)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Slime setup ;;

