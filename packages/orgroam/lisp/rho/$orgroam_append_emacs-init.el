;; package:orgroam ; please do not remove or edit these comments
;; orgroam ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path (concat rho--root-dir "/lisp/org-roam/compat"))
(add-to-list 'load-path (concat rho--root-dir "/lisp/org-roam/emacsql"))
(add-to-list 'load-path (concat rho--root-dir "/lisp/org-roam/emacsql-sqlite"))
(add-to-list 'load-path (concat rho--root-dir "/lisp/org-roam/magit-section"))
(add-to-list 'load-path (concat rho--root-dir "/lisp/org-roam/org-roam"))
(add-to-list 'load-path (concat rho--root-dir "/lisp/org-roam/org-roam-ui"))
(add-to-list 'load-path (concat rho--root-dir "/lisp/org-roam/simple-httpd"))
(add-to-list 'load-path (concat rho--root-dir "/lisp/org-roam/websocket"))

(setq org-roam-dir "~/org-roam")

(setq org-roam-db-location (file-truename (concat org-roam-dir "/org-roam.db")))

(unless rho--precomp?
  (condition-case err
    (progn
      (if (not (file-exists-p org-roam-dir))
        (make-directory org-roam-dir))

      (use-package org-roam
        :after org
        :init (setq org-roam-v2-ack t)
        :custom
        (org-roam-directory (file-truename org-roam-dir))
        :config
        (org-roam-setup)
        :bind (("C-c n f" . org-roam-node-find)
               ("C-c n r" . org-roam-node-random)		    
               (:map org-mode-map
                     (("C-c n i" . org-roam-node-insert)
                      ("C-c n o" . org-id-get-create)
                      ("C-c n t" . org-roam-tag-add)
                      ("C-c n a" . org-roam-alias-add)
                      ("C-c n l" . org-roam-buffer-toggle)))))
   
      (require 'org-roam-ui))

  (error (message "%s" (error-message-string err)))))


(rho--create-default-get-version 'orgroam)

(defun rho--browse-org-roam-docs ()
 (interactive)
 (browse-url "https://www.orgroam.com/manual.html"))

(easy-menu-add-item  nil '("((" "Browse Documentation")
   ["Org-roam" rho--browse-org-roam-docs t])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; orgroam ;;
