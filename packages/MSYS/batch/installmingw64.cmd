cd %RHO_DIR%\bin\msys
msys2 usr/bin/bash.exe -l -c "pacman -Sy --needed base-devel mingw-w64-x86_64-toolchain git subversion mercurial mingw-w64-x86_64-cmake mingw-w64-x86_64-boost"

