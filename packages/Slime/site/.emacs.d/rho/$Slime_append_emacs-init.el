;; package:Slime ; please do not remove or edit these comments
;; Slime setup ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *invocation-tag* (getenv "INVOCATION_TAG"))

(defconst +running-clojure+ (and *invocation-tag* 
                                 (string-match "Clojure" *invocation-tag*)))
                                           
(defvar *slime-path* (concat +rho-dir+ "/site/lisp/slime"))

;(when +running-clojure+
;  (setf *slime-path* (concat *slime-path* "-legacy")))

(add-to-list 'load-path *slime-path*)

(setq slime-net-coding-system 'utf-8-unix)
(require 'slime)

(setq slime-startup-animation t)
(slime-setup '(slime-fancy slime-banner slime-asdf))

(defun run-slime-repl (instance) 
  (interactive) 

;  (when (and (eq instance 'clojure) +running-clojure+)
;    (display-warning '(rho)
;     (concat "You're running Clojure 1.2 SLIME REPL. This REPL mode is not supported for " 
;             "Clojure 1.3 and further.\nUse the `run-clojure' command at the Emacs command shell or "
;             "the `lispx -l clojure --repl' command in your command prompt to run a Clojure 1.3 "
;             "command-line repl.\n"
;             "M-x clojure-jack-in command loads a Leiningen project from the current directory (if any).")))

  (cond ;((eq instance 'clojure)
        ; (setq slime-use-autodoc-mode nil)
        ; (slime-autodoc-mode 0)
        ; (if +running-clojure+
        ;     (progn
        ;       (display-warning '(rho)
        ;                        "Autodoc mode is turned off." :warning)
        ;       (split-frame #'(lambda () (slime instance))))
        ;   (display-warning '(rho)
        ;                    (concat "It's not possible to start a Clojure 1.2 REPL with the recent version of SLIME. \n"
        ;                            "Use the Windows shortcut to run the REPL with old version of slime, or \`run-clojure\' "
        ;                            "command from a Emacs shell to launch a Clojure 1.3 command-line REPL.\n"
        ;                            "M-x clojure-jack-in command loads a Leiningen project from the current directory (if any).") :error)))
                               
             
      ;((and (eq instance 'ecl) (>= +win-ver-major+ 6))
      ;  (display-warning '(rho)
      ;    "It's not possible to run ECL with SLIME using this version of Windows. Please use Emacs Command Shell or lispx-proxy." :error))
      ;((eq instance 'clojure-project)
      ; (split-frame #'(lambda () (call-interactively #'open-clojure-project))))
      (t (split-frame #'(lambda () (slime instance))))))

(defconst +lisp-home+ (concat +home-dir+ "/lisp"))
(defconst +quicklisp-home+ (concat +home-dir+ "/.quicklisp"))
(defconst +quicklisp-tag+ (concat +home-dir+ "/.quicklisp/.quicklisp/quicklisp.asd"))

(if (and (not +anyhome?+) (not (file-exists-p +lisp-home+)))
  (make-directory +lisp-home+))

(when (and (not +anyhome?+) (not (file-exists-p +quicklisp-tag+)))
  (copy-directory (concat +rho-dir+ "/site/lisp/.quicklisp") +quicklisp-home+))

;(when (require 'ac-slime)
;  (add-hook 'slime-mode-hook '(lambda () (set-up-slime-ac 'fuzzy)))
;  (add-hook 'slime-repl-mode-hook '(lambda () (set-up-slime-ac 'fuzzy))))

(create-default-get-version 'slime)

(defun rho-browse-slime-docs ()
 (interactive)
 (browse-url (concat "file://" +rho-dir+ "/docs/slime.pdf")))

(easy-menu-add-item nil '("((" "Browse Documentation") ["SLIME" rho-browse-slime-docs t])

(easy-menu-add-item  nil '("((")
  ["Clear Fasl Cache" rho-clear-fasl-cache t] "--")

(easy-menu-add-item  nil '("((")
  ["List Installed ASDF Systems"
					rho-list-installed-asdf-systems t] "--")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Slime setup ;;

