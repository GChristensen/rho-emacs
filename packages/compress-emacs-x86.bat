mklink /J .\emacs-x86\bin\lispx\ .\emacs-x64\bin\lispx
mklink /J .\emacs-x86\bin\utils\ .\emacs-x64\bin\utils
mklink /J .\emacs-x86\$bin\ .\emacs-x64\$bin
mklink /J .\emacs-x86\batch\ .\emacs-x64\batch
mklink /J .\emacs-x86\docs\ .\emacs-x64\docs
mklink /J .\emacs-x86\images\ .\emacs-x64\images
mklink /J .\emacs-x86\site\ .\emacs-x64\site
copy /Y .\emacs-x64\rho.conf .\emacs-x86\
copy /Y .\emacs-x64\rho.conf.portable .\emacs-x86\
copy /Y .\emacs-x64\rho.exe .\emacs-x86\
copy /Y .\emacs-x64\lispx.bat .\emacs-x86\
copy /Y .\emacs-x64\version\version.txt .\emacs-x86\version

@call 7za a -xr!.svn -mx9 "..\build\rho-emacs-x86-%1.pkg" .\emacs-x86\*

rmdir .\emacs-x86\bin\lispx
rmdir .\emacs-x86\bin\utils
rmdir .\emacs-x86\batch
rmdir .\emacs-x86\$bin
rmdir .\emacs-x86\docs
rmdir .\emacs-x86\images
rmdir .\emacs-x86\site 
del /Q .\emacs-x86\rho.conf 
del /Q .\emacs-x86\rho.conf.portable 
del /Q .\emacs-x86\rho.exe 
del /Q .\emacs-x86\lispx.bat 
del /Q .\emacs-x86\version\version.txt
