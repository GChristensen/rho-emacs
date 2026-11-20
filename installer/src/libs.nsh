# Plugins and NSIS libraries
# Part of the Rho Emacs installer, included from installer\rho.nsi

# Included files
!addplugindir "${INSTALLER_DIR}\plugins"
!addincludedir "${INSTALLER_DIR}\lib"

!include Sections.nsh
!include MUI2.nsh
#!include Trim.nsh
!include LogicLib.nsh
!include WinMessages.nsh
!include Locate.nsh
!include InstallOptions.nsh
!include FileFunc.nsh
!include FileAssotiation.nsh

var /GLOBAL switch_overwrite
!include MoveFileFolder.nsh

!include NSISpcre.nsh
!insertmacro REMatches
#!insertmacro un.REMatches

!include StrFunc.nsh
#${StrStr}
${StrStrAdv}
#${UnStrStrAdv}

#!include EnvVarUpdate.nsh
!include RadioButtons.nsh
