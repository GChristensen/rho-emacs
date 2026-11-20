# Repository tasks.
#
#   just                  list the available recipes
#   just compile-elisp    byte-compile every .el under packages/ with the bundled Emacs
#   just installer        build the installer (installer/rho.nsi) into the repository root
#
# The Emacs and NSIS locations can be overridden from the environment, e.g.
#   $env:EMACS_BIN = "C:\emacs\bin"; just compile-elisp
#   $env:NSIS_BIN = "C:\Program Files (x86)\NSIS"; just installer

set windows-shell := ["powershell.exe", "-NoLogo", "-NoProfile", "-Command"]

emacs_bin := env("EMACS_BIN", "software/emacs-x64/bin")
nsis_bin := env("NSIS_BIN", "D:/software/dev/nsis")

# list the available recipes
default:
    @just --list

# byte-compile all .el files under packages/ (.elc are written next to the sources)
compile-elisp:
    & "{{emacs_bin}}/emacs.exe" -Q --batch -l scripts/compile-el.el

# build the installer, rho-emacs-x86_64-setup-<version>.exe is written to the repository root
installer:
    & "{{nsis_bin}}/makensis.exe" installer/rho.nsi
