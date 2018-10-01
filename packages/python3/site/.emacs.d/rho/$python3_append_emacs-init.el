;; package:python3 ; please do not remove or edit these comments
;; Python setup ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst +python3+ t)
;(when (not (or +redist-2008+ (boundp +python2+)))
;  (princ "Installing Microsoft Visual C++ 2008 Redistributable")
;  (sit-for 1)
;  (w32-shell-execute "open" (replace-regexp-in-string 
;                              "/" (regexp-quote "\\")
;                              (concat +rho-dir+
;                                      "\\bin\\utils\\vcredist_x86.exe")) "/qb"))

(defun rho-run-python3 ()
  (interactive)
  (setq python-shell-completion-native-enable nil)
  (setq python-shell-prompt-detect-enabled nil)
  (setq python-shell-interpreter (concat +rho-dir-unix-path+ "/bin/python3/python.exe"))
  (switch-to-buffer (process-buffer (run-python nil t nil))))

(easy-menu-add-item nil '("((" "Launch") ["Python 3" rho-run-python3 t])

(defun rho-browse-python3-docs ()
 (interactive)
 (w32-shell-execute "open" (replace-regexp-in-string 
                            "/" (regexp-quote "\\")
                             (concat +rho-dir+ "/bin/python3/Doc/python361.chm"))))

(easy-menu-add-item nil '("((" "Browse Documentation") ["Python 3K" rho-browse-python3-docs t])

(create-default-get-version 'python3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Python setup ;;

