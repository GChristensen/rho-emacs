;; package:CollectBackups ; please do not remove or edit these comments
;; Additional modes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq backup-directory-alist `(("." . ,(concat rho--emacs-home "/backups"))))
(setq delete-old-versions t)
(setq kept-new-versions 5)
(setq kept-old-versions 2)
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms
      `((".*" ,(concat rho--emacs-home "/auto-save-list/") t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Additional modes ;;
