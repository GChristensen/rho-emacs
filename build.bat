@rem rho build script

if not exist ".\build" mkdir ".\build"

@set PATH=%~dp0packages\emacs-x64\$bin\emacs\EmacsW32\gnuwin32\bin;%~dp0packages\emacs-x64\$bin\emacs\EmacsW32\bin;%PATH%

set /p version= < .\packages\version.txt

@if "%1" == "installer" goto build_installer
@if "%1" == "basic" goto build_basic
@if "%1" == "basic32" goto build_basic32
@if "%1" == "packages" goto build_packages
@if "%1" == "debug" goto debug_installer
@if "%1" == "run" goto run_installer
@if "%1" == "launcher" goto build_launcher
@if "%1" == "all" goto build_all

@goto list_commands

:build_all

@rem ---------------------------------------------------------------------------
@rem build launcher

:build_launcher

cd .\launcher\Release

mingw32-make clean
mingw32-make all
@rem mingw32-make term

copy /Y "rho.exe"  "..\..\packages\emacs-x64\" 

mingw32-make clean

cd "..\.."

@if "%1" == "launcher" goto end

@rem ---------------------------------------------------------------------------
@rem compress packages

:build_packages

@rem @copy .\htdocs\index.shtml ".\packages\Emacs-x64\docs\rho\"

@cd .\packages

@call software-copy 
@call software-patch

@call compress

@call software-clean

@cd "..\"

@rem ---------------------------------------------------------------------------
@rem build installer

:build_installer

makensis setup.nsi

@goto end

@rem ---------------------------------------------------------------------------
@rem build basic installer

:build_basic

makensis /DBASIC_EMACS /DARCH_64 setup.nsi

@goto end

@rem ---------------------------------------------------------------------------
@rem build basic installer 32-bit

:build_basic32

makensis /DBASIC_EMACS /DARCH_32 setup.nsi

@goto end


@rem ---------------------------------------------------------------------------
@rem debug installer

:debug_installer

cd .\build

call rho-setup-%version%.exe /DEBUG

@goto end

@rem ---------------------------------------------------------------------------
@rem test installer

:run_installer

set /p version= < .\packages\version.txt

cd .\build

call rho-setup-%version%.exe /TEST

@goto end

@rem ---------------------------------------------------------------------------


:list_commands

@echo.
@echo Specify one of the following parameters:
@echo.
@echo   installer - build installer only
@echo   basic - build rho-lite-x86_64 installer
@echo   basic32 - build rho-lite-x86 installer
@echo   packages - build packages and installer
@echo   launcher - build launcher only
@echo   all - build complete distribution
@rem @echo   debug - run installer in debug mode
@echo   run - run installer in normal mode

:end