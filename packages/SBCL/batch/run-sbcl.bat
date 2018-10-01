@if not "%1" == "--no-asdf-install" goto run
@set NO_ASDF_INSTALL=t

:run
@"%RHO_DIR%\bin\sbcl\sbcl" --load "%RHO_DIR%\site\sbcl-init.lisp"