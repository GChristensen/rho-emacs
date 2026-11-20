;; package:CollectBackups ; please do not remove or edit these comments
;; Additional modes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Backslashes of a Windows HOME are escape characters in the replacement text
;; of `auto-save-file-name-transforms', so use forward slashes only
;; (92 is the backslash character).
(let ((emacs-home (subst-char-in-string 92 ?/ rho--emacs-home)))
  (setq backup-directory-alist `(("." . ,(concat emacs-home "/backups"))))
  (setq auto-save-file-name-transforms
        `((".*" ,(concat emacs-home "/auto-save-list/") t))))

(setq delete-old-versions t)
(setq kept-new-versions 5)
(setq kept-old-versions 2)
(setq version-control t)
(setq vc-make-backup-files t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Additional modes ;;
