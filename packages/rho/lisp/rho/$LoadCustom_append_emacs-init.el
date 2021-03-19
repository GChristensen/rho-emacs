;; package:LoadCustom ; please do not remove or edit these comments
;; Additional modes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq custom-file (concat rho--home-dir "/.emacs.d/.custom"))
(load custom-file t)


(defun rho-remove-persistent-warnings ()
  (message ""))

(run-with-idle-timer 1 nil 'rho-remove-persistent-warnings)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Additional modes ;;
