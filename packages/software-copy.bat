@set /p version= < version.txt
@for /D %%i in (*) do (if not exist  "..\build\rho-%%i-%version%.pkg"  @call .\%%i\$scripts\%%i-copy-software %%i)
