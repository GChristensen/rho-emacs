;; package:orgwiki ; please do not remove or edit these comments
;; orgwiki ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(unless rho--precomp?
  (condition-case err

    (use-package helm :ensure t
     :config
     (progn
      (require 'org-wiki)

      (setq org-wiki-location-list
            '(
              "~/org/wiki"
              ))

      (setq org-wiki-location (car org-wiki-location-list))

      (setq org-wiki-clip-jar-path (concat rho--root-dir "/utils/Clip.jar"))))
    
  (error (message "%s" (error-message-string err)))))


(rho--create-default-get-version 'orgwiki)

(defun rho--browse-org-wiki-docs ()
 (interactive)
 (browse-url "https://caiorss.github.io/org-wiki/"))

(easy-menu-add-item  nil '("((" "Browse Documentation")
   ["Org-wiki" rho--browse-org-wiki-docs t])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; orgwiki ;;
