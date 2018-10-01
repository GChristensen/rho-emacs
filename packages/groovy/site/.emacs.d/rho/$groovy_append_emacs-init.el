;; package:groovy ; please do not remove or edit these comments
;; groovy setup ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path (concat +rho-dir+ "/site/.emacs.d/groovy"))

(autoload 'groovy-mode "groovy-mode" "Mode for editing groovy source files" t)
(setq auto-mode-alist
      (append '(("\\.groovy\\'" . groovy-mode)) auto-mode-alist))
(setq interpreter-mode-alist (append '(("groovy" . groovy-mode))
                               interpreter-mode-alist))

(autoload 'run-groovy "inf-groovy" "Run an inferior Groovy process")
(autoload 'inf-groovy-keys "inf-groovy" "Set local key defs for inf-groovy in groovy-mode")

(add-hook 'groovy-mode-hook
      '(lambda ()
         (inf-groovy-keys)
))

(easy-menu-add-item nil '("((" "Launch") ["Groovy" run-groovy t])

(create-default-get-version 'groovy)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; groovy setup ;;

