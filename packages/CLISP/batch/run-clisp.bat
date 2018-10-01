@if not "%1" == "--no-asdf-install" goto run
@set NO_ASDF_INSTALL=t

:run
@"%RHO_DIR%\bin\clisp\clisp" -i "%RHO_DIR%\site\clisp-init.lisp"