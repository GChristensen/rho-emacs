@if not "%1" == "--no-asdf-install" goto run
@set NO_ASDF_INSTALL=t

:run

@if exist "%ProgramFiles(x86)%" goto win64

@"%RHO_DIR%\bin\ccl\wx86cl" --load "%RHO_DIR%\site\ccl-init.lisp"
@goto end

:win64

@"%RHO_DIR%\bin\ccl\wx86cl64" --load "%RHO_DIR%\site\ccl-init.lisp"

:end