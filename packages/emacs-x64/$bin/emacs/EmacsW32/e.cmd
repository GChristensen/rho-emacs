@rem Put this file (e.cmd) in your PATH.
@rem (Created by Setup Helper at Thu Jan 26 19:27:13 2012)
@rem -----------------------------
@rem Starts Emacs (through emacsclient) from command line.
@rem This can be used in many ways:
@rem    With a file as parameter: Start editing this file
@rem    With a directory as a parameter: Start dired
@rem If the first argument to this file is -e then there are
@rem some enhancements:
@rem    1) Before executing the -e command Emacs changes dir to current dir.
@rem    2) \ embedded in the -e parameter is converted to /.
@rem    3) The emacswindow is shown after eval.
@rem Then you can do for example to start ediff in Emacs do
@rem    e.cmd -e "(ediff-files \"fil1.txt\" \"fil 2.txt\")"
@setlocal
@set args=%*
@set emacs_client="%RHO_DIR%\bin\emacs\emacs\bin\emacsclient.exe"
@if not A%1 == A-e goto noE
@   set args=%args:\=/%
@   set args=%args:/"=\"%
@   set emacs_cd=%CD:\=/%
@   %emacs_client% -e "(setq default-directory \"%emacs_cd%\")"
:noE
@%emacs_client% -n %args%
@if not A%emacs_cd% == A %emacs_client% -n
