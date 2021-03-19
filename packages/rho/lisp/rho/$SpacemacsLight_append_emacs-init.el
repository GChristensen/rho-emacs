;; package:SpacemacsLight ; please do not remove or edit these comments
;; Additional modes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'custom-theme-load-path (concat custom-theme-directory "/spacemacs-theme/"))
(add-to-list 'load-path (concat custom-theme-directory "/spacemacs-theme/"))

(add-to-list 'load-path (concat rho--root-dir "/lisp/base/powerline"))
(add-to-list 'load-path (concat rho--root-dir "/lisp/base/spaceline"))

(custom-set-variables
 '(tabbar-separator (quote ("|")))
 '(spacemacs-theme-org-agenda-height nil)
 '(spacemacs-theme-org-height nil)
 '(spacemacs-theme-org-bold nil)
 '(spacemacs-theme-org-highlight nil)
 '(spacemacs-theme-underline-parens nil)
 '(custom-enabled-themes (quote (spacemacs-light)))
 '(powerline-default-separator (quote wave))
 )

(require 'spaceline-config)
(spaceline-emacs-theme)

(defun spacemacs-org-mode-hook ()
  (set-face-attribute 'org-level-1 nil :height 1.0)
  (set-face-attribute 'org-level-2 nil :height 1.0)
  (set-face-attribute 'org-level-3 nil :height 1.0)
  (set-face-attribute 'org-scheduled-today nil :height 1.0)
  (set-face-attribute 'org-agenda-date-today nil :height 1.1)
  (set-face-attribute 'org-table nil :foreground "#008787"))

(add-hook 'org-mode-hook 'spacemacs-org-mode-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Additional modes ;;

