@set RHO_DIR=%~dp0
@set RHO_DIR=%RHO_DIR:~0,-1%
@set EMACS_DIR=%RHO_DIR%/bin/emacs
set /p HOME=<"%RHO_DIR%/sandbox"
%RHO_DIR%/bin/emacs/bin/emacs.exe -nw %*