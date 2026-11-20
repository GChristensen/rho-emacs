;; package:LoadCustom ; please do not remove or edit these comments
;; Additional modes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq custom-file (concat rho--emacs-home "/.custom"))
(load custom-file t)


(defun rho-remove-persistent-warnings ()
  (message nil))

(run-with-idle-timer 1 nil 'rho-remove-persistent-warnings)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Additional modes ;;
