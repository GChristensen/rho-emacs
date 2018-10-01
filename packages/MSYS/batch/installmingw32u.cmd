cd %1
msys2 usr/bin/bash.exe -l -c "pacman --noconfirm -Sy --needed base-devel mingw-w64-i686-toolchain git subversion mercurial mingw-w64-i686-cmake mingw-w64-i686-boost"

