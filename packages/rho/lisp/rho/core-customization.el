;; -*- lexical-binding: t -*-

;; This file is a part of RHO Emacs installation
;; (C) 2021 g/christensen

(require 'easymenu)

(defun rho--edit-init-file ()
  (interactive)
  (find-file (concat rho--root-dir "/lisp/rho/emacs-init.el")))


(defface rho--default-face
  '((t :family "Tahoma"
       :slant normal))
  "")

(defun rho--do-browse-refcard (card)
 (browse-url (concat "file://" rho--root-dir "/docs/refcards/" card)))

(defmacro rho--browse-refcard (card)
  `(lambda () (interactive) (rho--do-browse-refcard ,card)))

;; Menu definition
(easy-menu-add-item  nil nil '("((") "File")

(easy-menu-add-item  nil '("((")
 `("Browse Documentation"
   ("Emacs Reference Cards" ["refcard.pdf" ,(rho--browse-refcard "refcard.pdf") t]
                      ["dired-ref.pdf" ,(rho--browse-refcard "dired-ref.pdf") t]
                      ["orgcard.pdf" ,(rho--browse-refcard "orgcard.pdf") t]
                      ["calccard.pdf" ,(rho--browse-refcard "calccard.pdf") t]
                      ["gnus-booklet.pdf" ,(rho--browse-refcard "gnus-booklet.pdf") t]
                      ["gnus-refcard.pdf" ,(rho--browse-refcard "gnus-refcard.pdf")t])))


(defun rho--browse-rho-docs ()
 (interactive)
 (browse-url "https://gchristensen.github.io/rho-emacs/"))

(easy-menu-add-item  nil '("((" "Browse Documentation")
   ["Rho Emacs" rho--browse-rho-docs t])


(defun rho--select-theme ()
 (interactive) (funcall 'customize-themes))

(easy-menu-add-item nil '("((") "--")
(easy-menu-add-item nil '("((")
                    ["Color Themes" rho--select-theme t])

(easy-menu-add-item nil '("((") ["--" nil t])
(easy-menu-add-item nil '("((")
                    ["Quit" save-buffers-kill-terminal t])

;; About menu and buffer
(defvar version-getters '())

(defun rho--get-version-info (component skip-lines)
  (with-temp-buffer
    (insert-file-contents
      (format "%s/version/%s_version.txt" rho--root-dir
              (symbol-name component)))
    (forward-line skip-lines)
    (buffer-substring (point) (point-max))))

(defun rho--create-default-get-version (component)
  (let ((component component))
    (setf version-getters
          (append version-getters
                  (list (lambda () (rho--get-version-info component 1)))))))

(defun rho--about-content ()
  (let* ((image-file (concat rho--root-dir "/images/logo.png"))
	 (img (create-image image-file))
	 (image-width (and img (car (image-size img))))
	 (window-width (window-width (selected-window)))
     (year (format-time-string "%Y"))
     (rho-version-string
          (format " v%s"
                  (with-temp-buffer
                    (insert-file-contents (concat rho--root-dir "/version/version.txt"))
                    (buffer-string)))))
    (when img
      (when (> window-width image-width)
        (insert (propertize " " 'display
                            `(space :align-to (+ center (-0.5 . ,img)))))
        (make-button (prog1 (point) (insert-image img)) (point)
                     'face 'default
                     'help-echo "https://gchristensen.github.io/rho-emacs"
                     'action (lambda (button) (browse-url "https://gchristensen.github.io/rho-emacs"))
                     'follow-link t)
        (insert "\n\n")))
    (insert-button "Rho Emacs"
                   'face (list 'link 'rho--default-face)
                   'action '(lambda (arg) (browse-url "https://gchristensen.github.io/rho-emacs"))
                   'help-echo "https://gchristensen.github.io/rho-emacs"
                   'follow-link t)
    (insert (propertize (concat rho-version-string "") 'face 'rho--default-face))
    (insert "\n")
    (insert (propertize (concat "(C) 2010-" year " g/christensen (") 'face 'rho--default-face))
    (insert-button "gchristnsn@gmail.com"
                   'face (list 'link 'rho--default-face)
                   'action '(lambda (arg) (browse-url "mailto:gchristnsn@gmail.com"))
                   'help-echo "mailto:gchristnsn@gmail.com"
                   'follow-link t)
    (insert (propertize ")\n" 'face 'rho--default-face))
    (insert (propertize "\nRho home directory: " 'face 'rho--default-face))
    (insert-button rho--home-dir
                   'face (list 'link 'rho--default-face)
                   'action '(lambda (arg) (dired rho--home-dir))
                   'help-echo rho--home-dir
                   'follow-link t)
    (insert (propertize "\n\nInstalled components:\n\n" 'face 'rho--default-face))
    (dolist (getter version-getters)
      (insert (propertize (concat "\u25A0 " (s-join "\n\u25A0 " (s-split "\n" (funcall getter)))) 'face 'rho--default-face))
      (insert (propertize "\n")))
    (goto-char (point-min))
    (forward-line))) 

(defun rho--show-about-buffer ()
  (interactive)
  (let ((about-buffer (get-buffer-create "*About Rho*"))
        (face 'variable-pitch))
    (switch-to-buffer about-buffer)
    (delete-other-windows)
    (when (not (fboundp 'salty))
	  (condition-case nil (load "salty")
      ('error nil)))
    (unless (when (fboundp 'salty) (salty))
      (rho--about-content))
    (toggle-read-only)))
                                                                      
(easy-menu-add-item  nil '("Help") '("--"))

(easy-menu-add-item  nil '("Help") 
  ["About Rho" rho--show-about-buffer t])


(rho--create-default-get-version 'emacs)

(defun rho--compile-extensions ()
  (switch-to-buffer "*Messages*")
  (byte-recompile-directory rho--lisp-dir 0)
  (let ((desktop-lock (concat rho--home-dir "/.emacs.desktop.lock")))
    (when (file-exists-p desktop-lock)
      (delete-file desktop-lock)))
  (setq kill-emacs-hook nil)
  (kill-emacs))

