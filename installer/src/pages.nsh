# Installer pages and languages
# Part of the Rho Emacs installer, included from installer\rho.nsi

!define MUI_HEADERIMAGE
!define MUI_HEADERIMAGE_BITMAP "${INSTALLER_DIR}\images\header.bmp"

# Installer pages
!define MUI_PAGE_CUSTOMFUNCTION_LEAVE leaveWelcome
!insertmacro MUI_PAGE_WELCOME
Page Custom CustomSettings leaveCustomSettings
!define MUI_PAGE_CUSTOMFUNCTION_LEAVE leaveDirectory
!insertmacro MUI_PAGE_COMPONENTS
!define MUI_PAGE_CUSTOMFUNCTION_LEAVE leaveComponents
!insertmacro MUI_PAGE_DIRECTORY
!define MUI_PAGE_CUSTOMFUNCTION_SHOW showInstFiles
!insertmacro MUI_PAGE_INSTFILES
!insertmacro MUI_PAGE_FINISH
!insertmacro MUI_UNPAGE_CONFIRM
!define MUI_PAGE_CUSTOMFUNCTION_SHOW un.showInstFiles
!insertmacro MUI_UNPAGE_INSTFILES

# Installer languages
!insertmacro MUI_LANGUAGE English
