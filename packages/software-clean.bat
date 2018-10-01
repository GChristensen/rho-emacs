@set /p version= < version.txt
@for /D %%i in (*) do (@call .\%%i\$scripts\%%i-clean-software %%i)
