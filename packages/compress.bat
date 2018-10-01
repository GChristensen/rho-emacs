@set /p version= < version.txt
@for /D %%i in (*) do (if not exist  "..\build\rho-%%i-%version%.pkg" @call compress-package.bat %%i %version%)
