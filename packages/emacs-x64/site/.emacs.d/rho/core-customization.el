;; This file is part of Rho installation
;; (C) 2010 g/christensen

(require 'easymenu)

;; Function determines frame creation policy after rho.exe invocation
(defun split-frame (&optional do-actions emacs-only)
  (flet ((get-frame ()
           (if emacs-only
	       (let ((dedicated-frame)
		     (list (frame-list)))
		 (while list
		   (let ((frame (car list)))
		     (when (frame-parameter frame 'emacs-dedicated-frame)
		       (setq dedicated-frame frame)
		       (setq list nil))
                     (setq list (cdr list))))
		 (if dedicated-frame
		     dedicated-frame
		     (let ((frame (new-frame)))
		       (set-frame-parameter frame 'emacs-dedicated-frame t)
		       frame)))
	     (new-frame)))
	 (do-split ()
	   (select-frame (get-frame))
	   (raise-frame)))
    (if (= (length (frame-list)) 1)
	(let ((frame (car (frame-list))))
	  (if (not (frame-parameter frame 'subsequent-invocation))
	      (progn
		(when emacs-only
		  (set-frame-parameter frame 'emacs-dedicated-frame t))
		(set-frame-parameter frame 'subsequent-invocation t))
	      (do-split)))
      (do-split)))
  (cd +home-dir+)
  (when do-actions (funcall do-actions))
  (delete-other-windows))

(defun visit-in-dedicated-frame (file-path)
  (lexical-let ((file-path file-path))
    (split-frame '(lambda ()
                    (when (not (string= file-path ""))
                      (find-file file-path)))
                 t))) 

;; Customize `shell' command
(defvar explicit-cmdproxy.exe-args nil)

(defun run-emacs-shell ()
  (interactive)
  (when (fboundp 'set-vc-env)
	(funcall 'set-vc-env))
  (let ((args explicit-cmdproxy.exe-args))
    (setq explicit-cmdproxy.exe-args '("/q" "/K \"echo Type `list-commands' to list available commands\""))
    (split-frame '(lambda () (call-interactively 'shell)))
    (setq explicit-cmdproxy.exe-args args)))

(defun rho-external-shell ()
  (interactive)
  (when (fboundp 'set-vc-env)
	(funcall 'set-vc-env))
  (start-process "rho-shell" "*rho-shell*"
                 (concat +rho-dir+ "/" +emacs-stem+ ".exe") "/TARGET:CMD" "/SHOW")
  (let ((kill-buffer-query-functions nil))
    (sleep-for 1)
    (kill-buffer "*rho-shell*")))

(defun rho-external-shell2 ()
  (rho-external-shell)
  (delete-file (concat +home-dir+ "/.emacs.d/.emacs.desktop.lock"))
  (setq kill-emacs-hook nil)
  (kill-emacs))

(defun rho-set-home-dir (path)
  (interactive (list (read-directory-name "Directory path: " default-directory)))
  (if (and (file-exists-p path) (file-directory-p path))
      (with-temp-file (concat +rho-dir+ "/sandbox")
        (prog1 (insert-string path)
               (princ "Please restart Emacs")))
      (princ "Path does not exist or not a directory")))

(defun rho-edit-init-file ()
  (interactive)
  (find-file (concat +rho-dir+
					 "/site/.emacs.d/rho/emacs-init.el")))


(defface rho-heading
  '((t :weight bold 
	   :family "Sans Serif"
	   :foreground "#FF6600"))
  "")

(iflisp

;; Add `Rho' menu group
(defun rho-clear-fasl-cache ()
  (interactive)
  (when (yes-or-no-p "Do you really want to clear fasl cache?")
    (delete-directory (concat +home-dir+ "/.fasl-cache") t)))

;; List available ASDF Systems
(defconst +sysdef-regexp+ "\\(?:\n\\|\r\\)(\\(?:asdf:\\)?defsystem\\*?[\t\n\r ]+?#?:?'?\"?\\([^\"\t\n\r ]*\\)")
(defconst +version-regexp+ ":version \"\\([^\"]*\\)")

(defun list-asdf-repository (repository)
  (mapcan
   (lambda (dir)
	 (when (and (file-directory-p dir) (not (string-match "\\.\\." dir)))
	   (let* ((asd-files (directory-files dir t ".*\\.asd"))
			  (get-match 
			   (lambda (regexp string)
				 (when (string-match regexp string)
				   (match-string 1 string))))
			  (get-sys-info 
			   (lambda (asd-file)
				 (with-temp-buffer 
				   (insert-file-contents asd-file)
				   (let ((def (buffer-substring (point) (point-max))))
                     (let ((name (funcall get-match +sysdef-regexp+ def)))
                       (when name
					    (list name
						      (funcall get-match +version-regexp+ def)
                              dir))))))))
		 (mapcar get-sys-info asd-files))))
   (directory-files repository t)))

(defun list-installed-asdf-systems ()
  (let ((systems 
		 (append (list-asdf-repository (concat +rho-dir+
											   "/site/lisp/repository"))
				 (list-asdf-repository (concat +home-dir+
											   "/lisp/repository"))
                 (let ((ql-systems-dir (concat +home-dir+ "/.quicklisp/dists/quicklisp/software")))
                   (when (file-exists-p ql-systems-dir)
				     (list-asdf-repository ql-systems-dir))))))
	(insert (propertize "The List of Available ASDF Systems\n\n" 
						'face 'rho-heading))
	(dolist (system (sort systems #'(lambda (l r) 
									  (string< (car l) (car r)))))
	  (when system
		(lexical-let ((system system)
					  (url (concat "http://cliki.net/" (car system))))
		  (insert-button (car system)
						 'face (list 'link face)
						 'action (lambda (arg) (browse-url url))
						 'help-echo url
						 'follow-link t)
		  (when (cadr system)
			(insert (propertize (concat " " (cadr system)) 
								'font 'variable-pitch)))
		  (insert (propertize " [" 'font 'variable-pitch))
		  (insert-button "dired"
						 'face (list 'link face)
						 'action (lambda (arg) (dired (caddr system)))
						 'help-echo (caddr system)
						 'follow-link t)
		  (insert (propertize "]" 'font 'variable-pitch))
		  (insert "\n"))))))

(defun rho-list-installed-asdf-systems ()
  (interactive)
  (let ((system-list-buffer (get-buffer-create "*Installed ASDF Systems*"))
        (face 'variable-pitch))
    (switch-to-buffer system-list-buffer)
    (delete-other-windows)
	(list-installed-asdf-systems)
	(goto-char (point-min))
    (forward-line)
    (toggle-read-only)))

(defun rho-browse-rho-docs ()
 (interactive)
 (browse-url (concat "file://" +rho-dir+ "/docs/Rho/index.shtml")))

(defun rho-browse-lispx-docs ()
 (interactive)
 (browse-url (concat "file://" +rho-dir+ "/docs/lispx-proxy/index.shtml")))

) ; iflisp

(defun rho-browse-emacsw32-docs ()
 (interactive)
 (browse-url (concat "file://" +rho-dir+ "/bin/emacs/EmacsW32/etc/emacsw32util.html")))

(defun rho-do-browse-refcard (card)
 (browse-url (concat "file://" +rho-dir+ "/docs/refcards/" card)))

(defmacro rho-browse-refcard (card)
  `(lambda () (interactive) (rho-do-browse-refcard ,card)))

;; Menu definition
(easy-menu-add-item  nil nil '("((") "File")

(iflisp
(easy-menu-add-item  nil '("((")
 '("Launch" ["Rho shell" run-emacs-shell t]
			["Rho shell (external)" rho-external-shell t]))
) ;iflisp

(easy-menu-add-item  nil '("((")
 `("Browse Documentation" 
   ("Emacs Reference Cards" ["refcard.pdf" ,(rho-browse-refcard "refcard.pdf") t]
                      ["dired-ref.pdf" ,(rho-browse-refcard "dired-ref.pdf") t]
                      ["orgcard.pdf" ,(rho-browse-refcard "orgcard.pdf") t]
                      ["calccard.pdf" ,(rho-browse-refcard "calccard.pdf") t]
                      ["gnus-booklet.pdf" ,(rho-browse-refcard "gnus-booklet.pdf") t]
                      ["gnus-refcard.pdf" ,(rho-browse-refcard "gnus-refcard.pdf")t])
   ["EmacsW32" rho-browse-emacsw32-docs t]))

(iflisp

 (easy-menu-add-item  nil '("((" "Browse Documentation")
   ["Rho Emacs" rho-browse-rho-docs t])
 (easy-menu-add-item  nil '("((" "Browse Documentation")
   ["lispx-proxy" rho-browse-lispx-docs t])

) ; iflisp

(easy-menu-add-item  nil '("((")
  ["Set Home Directory" rho-set-home-dir t])

(iflisp

(easy-menu-add-item  nil '("((")
  ["Clear Fasl Cache" rho-clear-fasl-cache t])

(easy-menu-add-item  nil '("((")
  ["List Installed ASDF Systems"
					rho-list-installed-asdf-systems t])

) ;iflisp

(defun rho-select-theme ()
 (interactive) (funcall 'customize-themes))

(easy-menu-add-item nil '("((") "--")
(easy-menu-add-item nil '("((")
                    ["Color Themes" rho-select-theme t])

(easy-menu-add-item nil '("((") ["--" nil t])
(easy-menu-add-item nil '("((")
                    ["Quit" save-buffers-kill-terminal t])

;; About menu and buffer
(defvar version-getters '())

(defun get-version-info (component skip-lines)
  (with-temp-buffer 
    (insert-file-contents 
      (format "%s/version/%s_version.txt" +rho-dir+
              (symbol-name component)))
    (forward-line skip-lines)
    (buffer-substring (point) (point-max))))

(defun create-default-get-version (component)
  (lexical-let ((component component))
    (setf version-getters 
          (append version-getters 
                  (list (lambda () (get-version-info component 1)))))))

(defun rho-about-content ()
  (let* ((image-file (concat +rho-dir+ "/images/logo.png"))
	 (img (create-image image-file))
	 (image-width (and img (car (image-size img))))
	 (window-width (window-width (selected-window)))
     (rho-version-string
          (format " v%s" 
                  (with-temp-buffer
                    (insert-file-contents (concat +rho-dir+ "/version/version.txt"))
                    (buffer-string)))))
    (when img
      (when (> window-width image-width)
        (insert (propertize " " 'display
                            `(space :align-to (+ center (-0.5 . ,img)))))
        (make-button (prog1 (point) (insert-image img)) (point)
                     'face 'default
                     'help-echo "http://rho-emacs.sourceforge.net"
                     'action (lambda (button) (browse-url "http://rho-emacs.sourceforge.net"))
                     'follow-link t)
        (insert "\n\n")))
    (insert-button "Rho Emacs"
                   'face (list 'link face)
                   'action '(lambda (arg) (browse-url "http://rho-emacs.sourceforge.net"))
                   'help-echo "http://rho-emacs.sourceforge.net"
                   'follow-link t)
    (insert (propertize (concat rho-version-string "") 'face face))
    (insert "\n")
    (insert (propertize "(C) 2010-2019 g/christensen (" 'face face))
    (insert-button "gchristnsn@gmail.com"
                   'face (list 'link face)
                   'action '(lambda (arg) (browse-url "mailto:gchristnsn@gmail.com"))
                   'help-echo "mailto:gchristnsn@gmail.com"
                   'follow-link t)
    (insert (propertize ")\n" 'face face))
    (insert (propertize "\nRho home directory: " 'face face))
    (insert-button +home-dir+
                   'face (list 'link face)
                   'action '(lambda (arg) (dired +home-dir+))
                   'help-echo +home-dir+
                   'follow-link t)
    (insert (propertize "\n\nInstalled components:\n\n" 'face face))
    (dolist (getter version-getters)
      (insert (propertize (concat "\u25A0 " (s-join "\n\u25A0 " (s-split "\n" (funcall getter)))) 'face face))
      (insert (propertize "\n")))
    (goto-char (point-min))
    (forward-line))) 

;(iflisp 

(defun show-about-buffer ()
  (interactive)
  (let ((about-buffer (get-buffer-create "*About Rho*"))
        (face 'variable-pitch))
    (switch-to-buffer about-buffer)
    (delete-other-windows)
    (when (not (fboundp 'salty))
	  (condition-case nil (load "salty")
      ('error nil)))
    (unless (when (fboundp 'salty) (salty))
      (rho-about-content))
    (toggle-read-only)))
                                                                      
(easy-menu-add-item  nil '("Help") '("--"))

(easy-menu-add-item  nil '("Help") 
  ["About Rho" show-about-buffer t])

;) ; iflisp

(create-default-get-version 'emacs)

(defun rho-compile-extensions ()
  (switch-to-buffer "*Messages*")
  (byte-recompile-directory +emacs-base-dir+ 0)
  (let ((desktop-lock (concat +home-dir+ "/.emacs.desktop.lock")))
    (when (file-exists-p desktop-lock)
      (delete-file desktop-lock)))
  (setq kill-emacs-hook nil)
  (kill-emacs))


(iflisp 

(when (not +anyhome?+)
 (let ((examples-dir (concat +home-dir+ "/.lispx/examples")))
  (when +precomp+
    (when (file-exists-p examples-dir)
      (delete-directory examples-dir t))
    (let ((examples-arc (concat +home-dir+ "/.lispx/lispx-proxy-examples.zip")))
      (when (file-exists-p examples-arc)
        (delete-file examples-arc)))

    (call-process "7za" nil nil nil "x" (concat +rho-dir+ "/bin/lispx/lispx-proxy-examples.7z")
                  (concat "-o" examples-dir) "-y"))

(when (not (file-exists-p examples-dir))
  (call-process "7za" nil nil nil "x" (concat +rho-dir+ "/bin/lispx/lispx-proxy-examples.7z")
                (concat "-o" examples-dir) "-y"))))

) ; iflisp
