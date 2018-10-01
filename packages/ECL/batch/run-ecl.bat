@if not "%1" == "--no-asdf-install" goto run
@set NO_ASDF_INSTALL=t

:run

@set PATH=%VC_INSTALL_DIR%\BIN;%VC_INSTALL_DIR%\PlatformSDK\BIN;%VC_INSTALL_DIR%\..\Common7\IDE;%WIN_SDK_DIR%\BIN;%PATH%
@set INCLUDE=%VC_INSTALL_DIR%\INCLUDE;%VC_INSTALL_DIR%\PlatformSDK\include;%WIN_SDK_DIR%\include
@set LIB=%RHO_DIR%\bin\ecl;%VC_INSTALL_DIR%\LIB;%VC_INSTALL_DIR%\PlatformSDK\lib;%WIN_SDK_DIR%\lib

@"%RHO_DIR%\bin\ecl\ecl.exe" -load "%RHO_DIR%\site\ecl-init.lisp"