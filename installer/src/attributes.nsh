# Installer attributes, constants and variables
# Part of the Rho Emacs installer, included from installer\rho.nsi

# Installer attributes
#!ifdef BASIC_EMACS
#!ifdef ARCH_32
#OutFile ".\${STEM}-emacs-x86-setup-${VERSION_SUFFIX}.exe"
#!else
OutFile ".\${STEM}-emacs-x86_64-setup-${VERSION_SUFFIX}.exe"
#!endif
#!else
#OutFile ".\${STEM}-setup-${VERSION_SUFFIX}.exe"
#!endif
InstallDir "$PROGRAMFILES64\${STEM}-emacs"
CRCCheck on
XPStyle on
ShowInstDetails hide
VIProductVersion "${VERSION}.0"
VIAddVersionKey ProductName "$(^Name)"
VIAddVersionKey ProductVersion "${VERSION}"
VIAddVersionKey FileVersion "${VERSION}"
VIAddVersionKey FileDescription ""
VIAddVersionKey LegalCopyright "${COPYRIGHT}"
InstallDirRegKey HKLM "${REGKEY}" Path
ShowUninstDetails hide

# Constants and variables
!define S_OK "OK"
!define S_FAIL "FAIL"

!define S_SETTINGS_TEXT "Installation options"
!define S_SETTINGS_SUBTEXT "Specify the location of personal files"

!define S_REMOVING_CURRENT_VER "Removing the currently installed version of \
    $(^Name)"

!define S_SUSPICIOUS_INSTDIR "Do you really want to install $(^Name) into the \
                              write-protected 'Program Files' folder?"

# Variables
var StartMenuGroup
var InstalledVersionPath
var PrivateEnvironment
var CommandParams
var SelectedHome
var UninstallPreviousVersion
