;; package:orgprotocol ; please do not remove or edit these comments
;; orgprotocol ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(unless rho--precomp?
  (require 'org-capture)
  (require 'org-protocol)
  (require 'org-protocol-capture-html)

  (custom-set-variables
   '(org-directory "~/org"))

  (setq org-default-notes-file (concat org-directory "/capture.org"))


  (defun rho-decode-capture-component (c)
    (let ((value (plist-get org-store-link-plist c)))
      (if value
          (decode-coding-string value locale-coding-system)
        "")))

  ;; add the templates without discarding the user's own ones
  (setf (alist-get "p" org-capture-templates nil nil #'equal)
        '("Protocol" entry (file "")
          "* %?[[%(rho-decode-capture-component :link)][%(rho-decode-capture-component :description)]] %U\n%(rho-decode-capture-component :initial)\n" :prepend t))
  (setf (alist-get "L" org-capture-templates nil nil #'equal)
        '("Protocol Link" entry (file "")
          "* %?[[%(rho-decode-capture-component :link)][%(rho-decode-capture-component :description)]] %U\n" :prepend t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; orgprotocol ;;
