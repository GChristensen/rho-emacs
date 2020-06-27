;; package:Clojure ; please do not remove or edit these comments
;; Clojure setup ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst +lein-home+ (concat +home-dir+ "/.lein"))
(defconst +clojure-user-dir+ (concat +home-dir+ "/clojure"))

(add-to-list 'load-path (concat +rho-dir+ "/site/.emacs.d/clojure"))

(custom-set-variables
  '(cider-lein-command (concat +lein-home+ "/lein")))
  
(when +precomp+
  (when (file-exists-p +lein-home+)
    (delete-directory +lein-home+ t t))
  (copy-directory (concat +rho-dir+ "/bin/clojure/.lein")
                  +lein-home+))
  
(when (and (not +anyhome?+) (not (file-exists-p +lein-home+)))
  (princ "Installing Clojure, please wait")
  (copy-directory (concat +rho-dir+ "/bin/clojure/.lein")
                  +lein-home+))

(unless +precomp+
  (condition-case err
   (progn
    (use-package clojure-mode :ensure t)

    (use-package cider :ensure t
     :config
     (progn
      (setf cider-repl-display-help-banner nil)

      (defun rho-run-clojure ()
        (interactive)
        (princ "Not implemented"))

      (if (not (file-exists-p +home-dir+))
        (make-directory +home-dir+))

      (if (and (not +anyhome?+) (not (file-exists-p +clojure-user-dir+)))
          (make-directory +clojure-user-dir+))

      (when (and (not +anyhome?+) (not (file-exists-p (concat +clojure-user-dir+ "/.repl"))))
        (copy-directory (concat +rho-dir+ "/bin/clojure/.repl")
                        (concat +clojure-user-dir+ "/.repl")))

      (if (and (not +anyhome?+) (not (file-exists-p (concat +clojure-user-dir+ "/lib"))))
        (make-directory (concat +clojure-user-dir+ "/lib")))

      ;; lost clojure java process workaround
      (add-hook 'kill-emacs-hook
                (lambda ()
                  (when (get-process "nrepl-server")
                      ;(cider-nrepl-request:eval "(System/exit 0)" (lambda ()))
                      (cider-quit)
                      ))))))

      (error (message "%s" (error-message-string err)))))

(easy-menu-add-item nil '("((" "Launch") ["Clojure (REPL)" 
       (lambda ()
        (interactive)
        (let ((path (concat +home-dir+ "/clojure/.repl")))
          (cd-absolute path)
          (let ((project-file (concat path "/project.clj")))
            (if (file-exists-p project-file)
              (cider-jack-in '())
              (princ (concat project-file " is missing"))))))


 t])

;(easy-menu-add-item nil '("((" "Launch") ["Clojure (open project)" 
;       (lambda (path)
;        (interactive (list
;                      (read-directory-name
;                       "Project root: "
;                       (if (functionp 'locate-dominating-file) ; Emacs 23 only
;                           (locate-dominating-file default-directory "src")
;                         default-directory))))          
;        (cd path)
;        (let ((project-file (concat path "/project.clj")))
;          (if (file-exists-p project-file)
;            (progn
;              (find-file project-file)
;              (cider-jack-in '()))
;            (princ "project.clj is missing"))))
; t])

(create-default-get-version 'clojure)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Clojure setup ;;

