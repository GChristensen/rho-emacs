# Custom page handlers
# Part of the Rho Emacs installer, included from installer\rho.nsi

# Page custom handlers

Function DetectInstalledVersion
  Push $0
  
  ReadRegStr $0 HKLM "${REGKEY}" Version
  ReadRegStr $InstalledVersionPath HKLM "${REGKEY}" Path    

  ${If} "" != $InstalledVersionPath
	StrCpy $UninstallPreviousVersion t
  ${EndIf}

  Pop $0
FunctionEnd

Function leaveWelcome
  Call DetectInstalledVersion
FunctionEnd

Function leaveComponents
FunctionEnd

Function CustomSettings

  !insertmacro MUI_HEADER_TEXT "${S_SETTINGS_TEXT}" "${S_SETTINGS_SUBTEXT}"
  !insertmacro INSTALLOPTIONS_EXTRACT_AS "${INSTALLER_DIR}\setup.ini" "setup.ini"
  
  ${If} $SelectedHome != ""
    !insertmacro INSTALLOPTIONS_WRITE "setup.ini" "Field 5" "State" $SelectedHome
  ${Else}
    ReadRegStr $0 HKCU "SOFTWARE\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders" Personal
    !insertmacro INSTALLOPTIONS_WRITE "setup.ini" "Field 5" "State" "$0\rho-emacs"
  ${EndIf}
  
  !insertmacro INSTALLOPTIONS_DISPLAY "setup.ini"  

FunctionEnd

Function leaveCustomSettings

  !insertmacro INSTALLOPTIONS_READ $0 "setup.ini" "Settings" "State"
  StrCmp $0 2 enable_input
  StrCmp $0 3 disable_input
  StrCmp $0 4 disable_input
  StrCmp $0 0 proceed
  Abort

enable_input:

  !insertmacro INSTALLOPTIONS_READ $0 "setup.ini" "Field 5" "HWND"
  EnableWindow $0 1
  Abort
  
disable_input:

  !insertmacro INSTALLOPTIONS_READ $0 "setup.ini" "Field 5" "HWND"
  EnableWindow $0 0
  Abort

proceed:

  # Check for a private environment location
  !insertmacro INSTALLOPTIONS_READ $0 "setup.ini" "Field 2" "State"
  !insertmacro INSTALLOPTIONS_READ $1 "setup.ini" "Field 4" "State"
  !insertmacro INSTALLOPTIONS_READ $SelectedHome "setup.ini" "Field 5" "State"
  
  ${If} $0 == 1
  
#resume_installation:

    StrCpy $PrivateEnvironment "sandbox"
  ${ElseIf} $1 == 1
    StrCpy $PrivateEnvironment "portable"
  ${EndIf}

  ${If} $UninstallPreviousVersion == "t"
  ${AndIf} $PrivateEnvironment != "portable"
    SectionGetFlags ${SEC_UNINSTALL} $0
    IntOp $0 $0 | ${SF_SELECTED}
    SectionSetFlags ${SEC_UNINSTALL} $0
  ${EndIf}

  StrCpy $INSTDIR "$PROGRAMFILES64\${STEM}-emacs"
  StrCmp $CommandParams "/TEST" leave_custom_debug leave_custom_finish

leave_custom_debug:

StrCpy $INSTDIR "D:\tmp\rho" 

leave_custom_finish:
FunctionEnd

Function leaveDirectory
FunctionEnd

Function showInstFiles
  !insertmacro SET_OUTPUT_PATH "$INSTDIR"
FunctionEnd

Function un.showInstFiles
FunctionEnd
