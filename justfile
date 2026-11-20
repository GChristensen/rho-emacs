# Repository tasks.
#
#   just                  list the available recipes
#   just compile-elisp    byte-compile every .el under packages/ with the bundled Emacs
#
# The Emacs location can be overridden from the environment, e.g.
#   $env:EMACS_BIN = "C:\emacs\bin"; just compile-elisp

set windows-shell := ["powershell.exe", "-NoLogo", "-NoProfile", "-Command"]

emacs_bin := env("EMACS_BIN", "software/emacs-x64/bin")

# list the available recipes
default:
    @just --list

# byte-compile all .el files under packages/ (.elc are written next to the sources)
compile-elisp:
    & "{{emacs_bin}}/emacs.exe" -Q --batch -l scripts/compile-el.el
