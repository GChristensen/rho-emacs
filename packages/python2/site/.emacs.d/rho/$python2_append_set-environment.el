;; package:python2 ; please do not remove or edit these comments
;; Environment setup ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setenv "PATH" (concat +rho-dir+ "/bin/python2/Scripts;" (getenv "PATH")))
(setenv "PATH" (concat +rho-dir+ "/bin/python2;" (getenv "PATH")))

(setenv "PATH" (concat +rho-dir+ "/bin/python2/Lib/site-packages/win32/lib;" (getenv "PATH")))

(setq python-python-command (concat +rho-dir+ "/bin/python2/python.exe"))
(setq py-python-command python-python-command)
(setenv "PYMACS_PYTHON" python-python-command)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Environment setup ;;
