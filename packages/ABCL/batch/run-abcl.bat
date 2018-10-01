@if not "%1" == "--no-asdf-install" goto run
@set NO_ASDF_INSTALL=t

:run
@java -Duser.home="%HOME%" -cp "%RHO_DIR%\bin\abcl\abcl-contrib.jar" -jar "%RHO_DIR%\bin\abcl\abcl.jar" --load "%RHO_DIR%\site\abcl-init.lisp"