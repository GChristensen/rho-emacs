cd %1
msys2 usr/bin/bash.exe -l -c "pacman -Sy --noconfirm --needed base-devel mingw-w64-x86_64-toolchain git subversion mercurial mingw-w64-x86_64-cmake mingw-w64-x86_64-boost"

