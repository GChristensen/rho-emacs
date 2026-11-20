# Core sections: shortcuts option, previous version removal, Emacs and Rho files
# Part of the Rho Emacs installer, included from installer\rho.nsi

Section /o "Desktop shortcuts" SEC_Shortcuts
    !insertmacro REG_STR HKLM "${REGKEY}\Components" Shortcuts 1
SectionEnd

ReserveFile "${INSTALLER_DIR}\setup.ini"

# Sections
Section /o -UninstallCurrent SEC_UNINSTALL
  DetailPrint "${S_REMOVING_CURRENT_VER}"
 
  SetDetailsPrint none
  ExecWait '"$InstalledVersionPath\uninstall.exe" /S _?=$InstalledVersionPath'
  RmDir /r $1
  SetDetailsPrint both
SectionEnd

Section -pre SEC_PRE
  # Debug config/apenders
  #SetOutPath $INSTDIR
  #File "packages\rho\rho.conf"
  #File "packages\rho\rhoc.conf"
  #SetOutPath "$INSTDIR\lisp\rho"
  #File /r "packages\rho\lisp\rho\"

  # Production
  SetOutPath "$INSTDIR\emacs"
  File /r "software\emacs-x64\"

  SetOutPath "$INSTDIR\"
  File /r "packages\rho\"
  File /r "elc\rho\"

  SetDetailsPrint both
  EnVar::SetHKCU
  EnVar::AddValue "PATH" "$INSTDIR"
  Pop $0

  !insertmacro CREATE_EMACS_SHORTCUT "" "Rho Emacs"

  StrCpy $9 ""
  Push $9
  Call WriteConfig

  StrCpy $9 "c"
  Push $9
  Call WriteConfig

SectionEnd
