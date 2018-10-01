cd %RHO_DIR%\bin\msys
msys2 usr/bin/bash.exe -l -c "pacman -Sy --needed base-devel mingw-w64-i686-toolchain git subversion mercurial mingw-w64-i686-cmake mingw-w64-i686-boost"

