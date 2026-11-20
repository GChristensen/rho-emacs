;; package:orgwiki ; please do not remove or edit these comments
;; orgwiki ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(unless rho--precomp?
  ;; helm and its dependencies are bundled, no network access is needed
  (dolist (dir '("async" "wfnames" "helm-core" "helm"))
    (add-to-list 'load-path (concat rho--root-dir "/lisp/helm/" dir)))

  (when (with-demoted-errors "orgwiki: %S"
          (require 'org-wiki)
          t)
    (setq org-wiki-location-list '("~/org/wiki"))
    (setq org-wiki-location (car org-wiki-location-list))
    (setq org-wiki-clip-jar-path (concat rho--root-dir "/utils/Clip.jar"))
    (rho--create-default-get-version 'orgwiki)))

(defun rho--browse-org-wiki-docs ()
 (interactive)
 (browse-url "https://caiorss.github.io/org-wiki/"))

(easy-menu-add-item  nil '("((" "Browse Documentation")
   ["Org-wiki" rho--browse-org-wiki-docs t])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; orgwiki ;;
