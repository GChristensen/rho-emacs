@rem Recompiles .el files given. Wildcard allowed.
@rem (Created by Setup Helper at Thu Jan 26 19:27:22 2012)
for %%f in (%*) do "%RHO_DIR%\bin\emacs\emacs\bin\emacs.exe" -batch -Q -L ./ -f batch-byte-compile %%f
