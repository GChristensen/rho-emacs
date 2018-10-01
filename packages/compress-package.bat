@if not "%1" == "emacs-x86" @call 7za a -xr!.svn -mx9 "..\build\rho-%1-%2.pkg" .\%1\*

@if not exist "..\build\rho-emacs-x86-%2.pkg" @call .\compress-emacs-x86 "%2"
