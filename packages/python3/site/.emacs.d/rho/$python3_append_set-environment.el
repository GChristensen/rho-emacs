;; package:python3 ; please do not remove or edit these comments
;; Environment setup ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setenv "PATH" (concat +rho-dir+ "/bin/python3/Scripts;" (getenv "PATH")))
(setenv "PATH" (concat +rho-dir+ "/bin/python3;" (getenv "PATH")))

(setenv "PATH" (concat +rho-dir+ "/bin/python3/Lib/site-packages/win32/lib;" (getenv "PATH")))

(setq python-python-command (concat +rho-dir+ "/bin/python3/python.exe"))
(setq py-python-command python-python-command)
(setenv "PYMACS_PYTHON" python-python-command)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Environment setup ;;

