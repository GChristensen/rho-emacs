;; package:python2 ; please do not remove or edit these comments
;; Python setup ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst +python2+ t)
;(when (not +redist-2008+)
;  (princ "Installing Microsoft Visual C++ 2008 Redistributable")
;  (sit-for 1)
;  (w32-shell-execute "open" (replace-regexp-in-string 
;                              "/" (regexp-quote "\\")
;                              (concat +rho-dir+
;                                      "\\bin\\utils\\vcredist_x86.exe")) "/qb"))

(defun rho-run-python2 ()
  (interactive)
  (setq python-shell-completion-native-enable nil)
  (setq python-shell-prompt-detect-enabled nil)
  (setq python-shell-interpreter (concat +rho-dir-unix-path+ "/bin/python2/python.exe"))
  (switch-to-buffer (process-buffer (run-python nil t nil))))

(easy-menu-add-item nil '("((" "Launch") ["Python 2" rho-run-python2 t])

(defun rho-browse-python2-docs ()
 (interactive)
 (w32-shell-execute "open" (replace-regexp-in-string 
                            "/" (regexp-quote "\\")
                             (concat +rho-dir+ "/bin/python2/Doc/python2713.chm"))))

(easy-menu-add-item nil '("((" "Browse Documentation") ["Python 2K" rho-browse-python2-docs t])


(create-default-get-version 'python2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Python setup ;;

