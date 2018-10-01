;; package:MzScheme ; please do not remove or edit these comments
;; MzScheme setup ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *mz-home* (concat +rho-dir+ "/bin/racket"))
(defvar *mz-doc-url* (concat "file://" (substitute ?/ ?\\ +rho-dir+)
                             "/bin/racket/doc"))

(add-to-list 'load-path (concat +rho-dir+ "/site/.emacs.d/racket"))
(add-to-list 'load-path (concat +rho-dir+ "/site/.emacs.d/racket/geiser"))
(add-to-list 'load-path (concat +rho-dir+ "/site/.emacs.d/racket/racket-mode"))

(setq
  quack-default-program (replace-regexp-in-string 
                          (regexp-quote "\\") (regexp-quote "\\\\")
                          (concat "\"" +rho-dir-win-path+
                                  "\\bin\\Racket\\racket\""))
  quack-fontify-style 'emacs
  quack-global-menu-p nil
  quack-run-scheme-always-prompts-p nil
  quack-run-scheme-prompt-defaults-to-last-p t
  quack-tabs-are-evil-p t
  quack-programs nil
  scheme-mit-dialect nil)

(require 'geiser)
(require 'quack)
(require 'racket-mode)

(add-to-list 'auto-mode-alist '("\\.rkt\\'" . racket-mode))

(custom-set-variables
 '(geiser-default-implementation (quote racket)))

(add-hook 'racket-mode-hook 'geiser-mode)

(setq quack-manuals
  `((r6rs "R6RS" ,(concat *mz-doc-url* "/r6rs-std/index.html") nil)
    (r6rs-lib "R6RS Library" ,(concat  *mz-doc-url* "/r6rs-lib-std/index.html") nil)
    (mz-reference "Racket: Language Reference" 
                  ,(concat *mz-doc-url* "/reference/index.html") nil)
    (plt "PLT Scheme Documentation (Web)" 
         "http://download.plt-scheme.org/doc/html/" nil)))

;; Default `quack-view-keyword-docs' function kills query part of an url opened 
;; through file:// protocol. This override attempts to get path to default 
;; browser and launch it with url passed trough command line parametes
(defvar *split-bound* (+ 5 (string-match ".exe." +browse-command+)))
(defvar *browser* (substring +browse-command+ 0 *split-bound*))
(defvar *params* (substring +browse-command+ *split-bound*))

(defun quack-view-keyword-docs (keyword)
  (interactive (list (quack-prompt-for-keyword "View docs for keyword")))
  (when (and keyword (stringp keyword) (not (string= keyword "")))
    (let* ((url (concat *mz-doc-url* 
			"/search/index.html?q=" 
			keyword))
	   (expanded-params
	    (if (string-match-p "%1" *params*)
		(replace-regexp-in-string "%1" url *params*)
	      (concat *params* " " url))))
      (w32-shell-execute nil *browser* expanded-params))))

(defun run-mzscheme-repl ()
  (split-frame #'(lambda () (interactive) (run-geiser 'racket))))

(defun rho-run-racket ()
  (interactive)
  (run-mzscheme-repl))

(easy-menu-add-item nil '("((" "Launch") ["Racket (Geiser REPL)" rho-run-racket t])

(defun rho-browse-racket-docs ()
 (interactive)
 (w32-shell-execute "open" (replace-regexp-in-string 
                            "/" (regexp-quote "\\")
                             (concat +rho-dir+ "/bin/racket/doc/index.html"))))

(easy-menu-add-item nil '("((" "Browse Documentation") ["Racket" rho-browse-racket-docs t])

(create-default-get-version 'mzscheme)

(defun kill-racket-gently ()
  (dolist (process (process-list))
	(when (string= (process-name process) "scheme")
	  (process-send-eof process))))

(add-to-list 'kill-emacs-hook 'kill-racket-gently)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MzScheme setup ;;

