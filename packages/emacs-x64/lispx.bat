@set RHO_DIR=%~dp0
@set RHO_DIR=%RHO_DIR:~0,-1%

@set LC_ALL=C

@set SBCL_HOME=%RHO_DIR%\bin\sbcl

@if not exist "%RHO_DIR%\sandbox" goto env_home

@set /p SANDBOX=<"%RHO_DIR%\sandbox"

@if "%SANDBOX%" == "" goto empty_home

@if not "%SANDBOX:~1,1%" == ":" goto relative_home

@set HOME=%SANDBOX%
@goto home_set

:relative_home

@set HOME=%RHO_DIR%\%SANDBOX%
@goto home_set

:env_home

@rem @if "%HOME%" == "" goto empty_home
@rem @goto home_set

:empty_home

@set PATH=%RHO_DIR%\bin\emacs\EmacsW32\bin;%PATH%
@for /f "tokens=1*" %%a in ('w32-reg-iface.exe -r "HKCU\Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders\Personal"') do @set MYDOCUMENTS=%%b 
@if "%MYDOCUMENTS:~-1%" == " " set MYDOCUMENTS=%MYDOCUMENTS:~0,-1%

@set HOME=%MYDOCUMENTS%\rhome

:home_set

@set NO_ASDF_INSTALL=t

@set PATH=%RHO_DIR%\bin\openssl;%PATH%

@set LISPX_DIR=%RHO_DIR%\bin\lispx

@set PATH=%LISPX_DIR%;%PATH%
@for /f "tokens=*" %%a in ('home-utf-8.bat') do @set HOME_UTF8=%%a

@call "%LISPX_DIR%\set-environment.bat"

@if "%INVOCATION_TAG%" == "TARGET:LISPX" goto gui_subsystem

@call "%LISPX_DIR%\lispx.exe" %*
@goto end

:gui_subsystem

@call "%LISPX_DIR%\lispx-gui.exe" %*

:end