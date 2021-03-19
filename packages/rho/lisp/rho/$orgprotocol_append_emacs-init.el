;; package:orgprotocol ; please do not remove or edit these comments
;; orgprotocol ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(unless rho--precomp?
  (require 'org-protocol)
  (require 'org-protocol-capture-html)

  (custom-set-variables
   '(org-directory "~/org"))

  (setq org-default-notes-file (concat org-directory "/capture.org"))


  (defun rho-decode-capture-component (c)
    (decode-coding-string (plist-get org-store-link-plist c) 
                           locale-coding-system))


  (setq org-capture-templates '())
    (add-to-list 'org-capture-templates
                 '("p" "Protocol" entry (file "")
                   "* %?[[%(rho-decode-capture-component :link)][%(rho-decode-capture-component :description)]] %U\n%(rho-decode-capture-component :initial)\n" :prepend t))
    (add-to-list 'org-capture-templates
                 '("L" "Protocol Link" entry (file "")
                   "* %?[[%(rho-decode-capture-component :link)][%(rho-decode-capture-component :description)]] %U\n" :prepend t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; orgprotocol ;;
