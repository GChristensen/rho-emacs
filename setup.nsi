# Rho Emacs installer                    
# (C) 2018 g/christensen 

Name "Rho Emacs"
!define STEM "rho"
!define _SUFFIX ""

!ifndef BASIC_EMACS
SetCompressor /SOLID lzma
!endif
RequestExecutionLevel admin

# General Symbol Definitions
!define VERSION 0.3.0
!define VERSION_SUFFIX ${VERSION}
!define REGKEY "SOFTWARE\$(^Name)"
BrandingText "$(^Name) v${VERSION}"

# MUI Symbol Definitions
#!ifdef BASIC_EMACS
!define MUI_ICON "${NSISDIR}\Contrib\Graphics\Icons\modern-install-blue.ico"
#!else
#!define MUI_ICON "${NSISDIR}\Contrib\Graphics\Icons\orange-install.ico"
#!endif
!define MUI_FINISHPAGE_NOAUTOCLOSE
!define MUI_UNICON "${NSISDIR}\Contrib\Graphics\Icons\modern-uninstall-full.ico"
!define MUI_UNFINISHPAGE_NOAUTOCLOSE
!define MUI_WELCOMEFINISHPAGE_BITMAP ".\nsis\images\side_b.bmp"
!define MUI_UNWELCOMEFINISHPAGE_BITMAP "${NSISDIR}\Contrib\Graphics\Wizard\orange-uninstall.bmp"

# Included files
!addplugindir .\nsis\plugins
!addincludedir .\nsis\lib

!include Sections.nsh
!include MUI2.nsh
!include Trim.nsh
!include LogicLib.nsh
!include WinMessages.nsh
!include Locate.nsh
!include InstallOptions.nsh
!include WinMessages.nsh
!include FileFunc.nsh
!include FileAssotiation.nsh

var /GLOBAL switch_overwrite
!include MoveFileFolder.nsh

!include NSISpcre.nsh
!insertmacro REMatches
#!insertmacro un.REMatches

!include StrFunc.nsh
${StrStr}
${StrStrAdv}
#${UnStrStrAdv}

!include EnvVarUpdate.nsh
!include RadioButtons.nsh

!define MUI_HEADERIMAGE
#!ifdef BASIC_EMACS
#!define MUI_HEADERIMAGE_BITMAP "${NSISDIR}\Contrib\Graphics\Header\win.bmp"
#!else
!define MUI_HEADERIMAGE_BITMAP ".\nsis\images\header.bmp"
#!endif

# Installer pages
!define MUI_PAGE_CUSTOMFUNCTION_LEAVE leaveWelcome
!insertmacro MUI_PAGE_WELCOME
!define MUI_PAGE_CUSTOMFUNCTION_LEAVE leaveComponents
!insertmacro MUI_PAGE_COMPONENTS
Page Custom CustomSettings leaveCustomSettings
!define MUI_PAGE_CUSTOMFUNCTION_LEAVE leaveDirectory
!insertmacro MUI_PAGE_DIRECTORY
!define MUI_PAGE_CUSTOMFUNCTION_SHOW showInstFiles
!insertmacro MUI_PAGE_INSTFILES
!insertmacro MUI_PAGE_FINISH
!insertmacro MUI_UNPAGE_CONFIRM
!define MUI_PAGE_CUSTOMFUNCTION_SHOW un.showInstFiles
!insertmacro MUI_UNPAGE_INSTFILES

# Installer languages
!insertmacro MUI_LANGUAGE English

# Installer attributes
!ifdef BASIC_EMACS
!ifdef ARCH_32
OutFile ".\build\${STEM}-emacs-x86-setup-${VERSION_SUFFIX}.exe"
!else
OutFile ".\build\${STEM}-emacs-x86_64-setup-${VERSION_SUFFIX}.exe"
!endif
!else
OutFile ".\build\${STEM}-setup-${VERSION_SUFFIX}.exe"
!endif
InstallDir "$PROGRAMFILES\${STEM}"
CRCCheck on
XPStyle on
ShowInstDetails hide
VIProductVersion "${VERSION}.0"
VIAddVersionKey ProductName "$(^Name)"
VIAddVersionKey ProductVersion "${VERSION}"
VIAddVersionKey FileVersion "${VERSION}"
VIAddVersionKey FileDescription ""
VIAddVersionKey LegalCopyright "(C) 2017 g/christensen"
InstallDirRegKey HKLM "${REGKEY}" Path
ShowUninstDetails hide

# Component archives
!define DL_HOST "http://downloads.sourceforge.net/rho-emacs"
!define VERSION_INFO "version.info"
!define INSTALLER_STEM "${STEM}-setup"
!define ARC_EXT "pkg"

# Constants and variables
!define S_OK "OK"
!define S_FAIL "FAIL"

!define S_DOWNLOAD_FAILED "A component failed to download. Please see \
                           installation details."
!define S_INSTALLATION_FAILED "A component could not be extracted. Please check \
                           if it was downloaded correctly."
!define S_SETTINGS_TEXT "Installation options"
!define S_SETTINGS_SUBTEXT "Specify additional installation information"    

!define S_REMOVING_CURRENT_VER "Removing the currently installed version of \
    $(^Name)"
!define S_UPDATE_REQUEST "There is a more recent version of $(^Name) \
                          available. Would you like to download and launch \
                          the recent installer?"

!define S_SUSPICIOUS_INSTDIR "Do you really want to install $(^Name) into the \
                              write-protected 'Program Files' folder?"

!define S_LISPX_SCRIPT "LISPX Script"
!define S_LISPX_DISTRIBUTION "LISPX Packaged Distribution"
!define S_WARN_SPACES "Because the home directory path contains spaces, \
                       some $(^Name) features will not be available. \
                       Do you really want to continue?"

!define S_WARN_SPACES_INST "Because the installation directory path contains \
                            spaces, some $(^Name) features will not be \
                            available. Do you really want to continue?"

#!define S_CREATE_RESTORE_POINT "Creating system restore point"
 
# Variables
var StartMenuGroup
var InstalledVersionPath
var PrivateEnvironment
var ExecutionState
!ifndef BASIC_EMACS
var InstallSlime
!endif
var CommandParams
#var HomePath
var DoCheckForRecentVersion
var CurrentSandbox
var InstDirRedirect
var UninstallPreviousVersion

# Environment variable manipulation
!define ENV_HKLM 'HKLM "SYSTEM\CurrentControlSet\Control\Session Manager\
                 \Environment"'
!define ENV_HKCU 'HKCU "Environment"'

!macro SET_ENV_VAR Key EnvVar Val
  WriteRegExpandStr "${Key}" ${EnvVar} ${Val}
  SendMessage ${HWND_BROADCAST} ${WM_WININICHANGE} 0 "STR:Environment" \
              /TIMEOUT=5000
!macroend

!macro UNSET_ENV_VAR Key EnvVar
  DeleteRegValue "${Key}" EnvVar
  SendMessage ${HWND_BROADCAST} ${WM_WININICHANGE} 0 "STR:Environment" \
             /TIMEOUT=5000
!macroend

!macro REG_STR root_key subkey key_name value
  ${If} $PrivateEnvironment != portable
    WriteRegStr ${root_key} "${subkey}" "${key_name}" "${value}"
  ${EndIf}
!macroend

# Error tracking
Function CheckExecutionState
  Pop $0
  ${If} $ExecutionState != S_OK
    MessageBox MB_OK|MB_ICONEXCLAMATION $0
    Abort
  ${EndIf}
FunctionEnd

!macro CHECK_EXECUTION_STATE Message
  Push "${Message}"
  Call CheckExecutionState
!macroend

# Configuration file manipulation
Function AppendConfig
  Pop $6 # interjection
  Pop $7 # component name
  Pop $8 # config name
  Pop $9 # dir
  
  !define CONFIG "$8"
  !define APPENDIX "$$$7_append$6_$8"
  !define DIR $9

  IfFileExists "${DIR}\${APPENDIX}" 0 go_out

  Push $0 # config handle
  Push $1 # appendix handle
  Push $2 # config row
  
  ClearErrors
  FileOpen $0 "${DIR}\${CONFIG}" a
  FileRead $0 $2
  ${DoUntil} ${Errors}
    ${If} $2 =~ "package:$7"      
      GoTo cleanup
    ${EndIf}
    FileRead $0 $2
  ${LoopUntil} 1 = 0
  
  ClearErrors
  FileOpen $1 "${DIR}\${APPENDIX}" r
  ${DoUntil} ${Errors}
    FileRead $1 $2
    FileWrite $0 $2
  ${LoopUntil} 1 = 0
  FileClose $1   
    
cleanup:
  FileClose $0
  
  Delete /REBOOTOK "${DIR}\${APPENDIX}" 

  Pop $2
  Pop $1
  Pop $0
go_out:

  !undef CONFIG
  !undef APPENDIX
  !undef DIR
FunctionEnd

!macro APPEND_CONFIG Comp Conf Dir
  SetDetailsPrint none
  Push ${Dir}
  Push ${Conf}
  Push ${Comp}
  Push ""
  
  Call AppendConfig
  SetDetailsPrint both
!macroend

!macro APPEND_CONFIG2 Comp Conf Dir
  SetDetailsPrint none
  Push ${Dir}
  Push ${Conf}
  Push ${Comp}
  Push "2"
  
  Call AppendConfig
  SetDetailsPrint both
!macroend

/*Function un.StripConfig
  Pop $7 # component name
  Pop $8 # config name
  Pop $9 # dir

  !define CONFIG "$8"
  !define COMP "$7"
  !define DIR $9

  Push $0 # config handle
  Push $1 # config row
  Push $2 # skip state
  Push $3 # section label
  Push $4 # temp handle
  Push $5 # strip performed

  StrCpy $2 f
  StrCpy $5 f
  
  ClearErrors
  FileOpen $0 "${DIR}\${CONFIG}" r
  IfErrors cleanup
  FileOpen $4 "${DIR}\${CONFIG}_${COMP}.tmp" a
  FileRead $0 $1 
  ${DoUntil} ${Errors}
    ${If} $2 != t
    ${AndIfNot} $1 un.!~ "package:${COMP}"       
      FileRead $0 $1
      IfErrors cleanup
      ${UnStrStrAdv} $3 $1 " " < < 0 0 0
   
      StrCpy $2 t
      FileRead $0 $1
      ${Continue}
    ${EndIf}
    
    ${If} $2 == t
      ${IfNot} $1 un.!~ $3
        StrCpy $2 f
        StrCpy $5 t
        FileRead $0 $1
        ${If} $1 != "$\r$\n"
          ${Continue}
        ${EndIf}
      ${EndIf}
    ${Else}
      FileWrite $4 $1
    ${EndIf}
    
    FileRead $0 $1
  ${LoopUntil} 1 = 0
  
cleanup:
  FileClose $0
  FileClose $4 
  
  ${If} $5 == t
    StrCpy $switch_overwrite 1
    !insertmacro MoveFile "${DIR}\${CONFIG}_${COMP}.tmp" "${DIR}\${CONFIG}"
  ${Else}
    Delete "${DIR}\${CONFIG}_${COMP}.tmp"
  ${EndIf} 
  
  !undef CONFIG
  !undef COMP
  !undef DIR

  Pop $5
  Pop $4
  Pop $3
  Pop $2
  Pop $1
  Pop $0 
FunctionEnd

!macro UNAPPEND_CONFIG Comp Conf Dir
  Push ${Dir}
  Push ${Conf}
  Push ${Comp}
  
  Call un.StripConfig
!macroend*/

/*Function PortableInstallJRE
  Push $0

  ReadRegStr $0 HKLM "SOFTWARE\JavaSoft\Java Runtime Environment\1.7" JavaHome

  ${If} $0 == ""
    ReadRegStr $0 HKLM "SOFTWARE\JavaSoft\Java Runtime Environment\1.6" JavaHome
  ${EndIf}                                                                    
  
  ${If} $0 == ""
    ReadRegStr $0 HKLM "SOFTWARE\JavaSoft\Java Runtime Environment\1.5" JavaHome
  ${EndIf}                                                                    

  DetailPrint JavaHome
  DetailPrint $0

  ${If} $0 != ""
    CreateDirectory "$InstDirRedirect\bin\jre"
    CopyFiles /SILENT "$0\*" "$InstDirRedirect\bin\jre"
  ${EndIf}

  Pop $0
FunctionEnd*/

Function CheckForTheRecentVersion
  ${If} $DoCheckForRecentVersion == y
    inetc::get /POPUP "" /CAPTION "$(^Name)" \
               "${DL_HOST}/${VERSION_INFO}" "$TEMP\${VERSION_INFO}"  
  
    IfFileExists "$TEMP\${VERSION_INFO}" 0 go_out

    Push $0
    Push $1
  
    ClearErrors
    FileOpen $0 "$TEMP\${VERSION_INFO}" r
    FileRead $0 $1
    FileClose $0
    SetDetailsPrint none
    Delete "$TEMP\${VERSION_INFO}"    
    SetDetailsPrint both
    
    ${If} $1 != "${VERSION}"
      MessageBox MB_YESNO|MB_ICONQUESTION "${S_UPDATE_REQUEST}" IDYES proceed \
                                                                 IDNO exit
    ${EndIf}  

    goto exit
    
proceed:
    ClearErrors
    inetc::get /RESUME "Dwnload failed. Retry download?" /POPUP "" \
               /CAPTION "$(^Name)" \
               "${DL_HOST}/${INSTALLER_STEM}-$1.exe" \ 
               "$EXEDIR\${INSTALLER_STEM}-$1.exe"
                                 
    IfFileExists "$EXEDIR\${INSTALLER_STEM}-$1.exe" 0 exit

    Exec '"$EXEDIR\${INSTALLER_STEM}-$1.exe"'
    Quit
  ${EndIf}
exit:
    Pop $1
    Pop $0
  
go_out:
  StrCpy $DoCheckForRecentVersion n
FunctionEnd

# Package downloading logic
!macro LOAD_PACKAGE Comp
  StrCmp $CommandParams "/DEBUG" lp_go_out_${Comp}

  ${If} ${SectionIsSelected} ${SEC_${Comp}}
    IfFileExists "$EXEDIR\${STEM}-${Comp}-${VERSION_SUFFIX}.${ARC_EXT}" lp_go_out_${Comp}
      call CheckForTheRecentVersion 
      DetailPrint "Downloading package ${STEM}-${Comp}-${VERSION_SUFFIX}.${ARC_EXT}" 
       inetc::get /RESUME "Dwnload failed. Retry download?" /POPUP "" \
                  /CAPTION "$(^Name)" \
                  "${DL_HOST}/${STEM}-${Comp}-${VERSION_SUFFIX}.${ARC_EXT}" "$EXEDIR\${STEM}-${Comp}-${VERSION_SUFFIX}.${ARC_EXT}"                  
      Pop $0
      ${If} $0 != ${S_OK}
        DetailPrint "${STEM}-${Comp}-${VERSION_SUFFIX}.${ARC_EXT} download failed"
        StrCpy $ExecutionState S_FAIL 
        Abort
      ${Else}
        DetailPrint "${STEM}-${Comp}-${VERSION_SUFFIX}.${ARC_EXT} download finished"
      ${EndIf} 
  ${EndIf}
  
lp_go_out_${Comp}:
!macroend

# Package installation logic
!macro SET_OUTPUT_PATH Path
  SetDetailsPrint none
  SetOutPath "${Path}"
  SetDetailsPrint both
!macroend

Function MergeLocation_
  ${StrStrAdv} $R1 $R9 $R0 > > 0 0 0
  ${StrStrAdv} $R2 $R9 $R0 > < 0 0 0
  
  SetDetailsPrint none
  !insertmacro MoveFolder $R9 "$R2$R1" "*.*"
  SetDetailsPrint both
  Push $R0
FunctionEnd

!macro EXTRACT_PACKAGE Comp Dir
  ${If} ${SectionIsSelected} ${SEC_${Comp}}
    ${If} $CommandParams == "/DEBUG"
      CopyFiles /SILENT "$EXEDIR\..\src\packages\${Comp}\*" ${Dir}
    ${Else}
!ifdef BASIC_EMACS
      IfFileExists "${Dir}\${STEM}-${Comp}-${VERSION_SUFFIX}.${ARC_EXT}" 0 ea_report_error    
        !insertmacro SET_OUTPUT_PATH ${Dir}
        Nsis7z::ExtractWithDetails "${Dir}\${STEM}-${Comp}-${VERSION_SUFFIX}.${ARC_EXT}" "Installing package %s..."
      Delete "${Dir}\${STEM}-${Comp}-${VERSION_SUFFIX}.${ARC_EXT}"
!else 
      IfFileExists "$EXEDIR\${STEM}-${Comp}-${VERSION_SUFFIX}.${ARC_EXT}" 0 ea_report_error    
        !insertmacro SET_OUTPUT_PATH ${Dir}
        Nsis7z::ExtractWithDetails "$EXEDIR\${STEM}-${Comp}-${VERSION_SUFFIX}.${ARC_EXT}" "Installing package %s..."
!endif
    ${EndIf}
    IfFileExists "${Dir}\version\${Comp}_version.txt" 0 ea_report_error
    StrCpy $R0 "$$"
    ${Locate} "${Dir}" "/L=D /M=$$* /G=0" "MergeLocation_"
    GoTo ea_go_out 
  ${Else}
    GoTo ea_go_out 
  ${EndIf}
  
ea_report_error:

  DetailPrint "${STEM}-${Comp}-${VERSION_SUFFIX}.${ARC_EXT} installation failed"
  StrCpy $ExecutionState S_FAIL 

ea_go_out:
!macroend

!macro INSTALL_PACKAGE Comp Size
  AddSize ${Size}
  SetShellVarContext all
  
  DetailPrint "Installing package: ${STEM}-${Comp}-${VERSION_SUFFIX}.${ARC_EXT}"
  IfFileExists "$InstDirRedirect\version\${Comp}_version.txt" ip_report_already_installed
  !insertmacro EXTRACT_PACKAGE ${Comp} $InstDirRedirect
  !insertmacro CHECK_EXECUTION_STATE "${S_INSTALLATION_FAILED}"
  !insertmacro APPEND_CONFIG ${Comp} ${STEM}.conf $InstDirRedirect
  !insertmacro APPEND_CONFIG ${Comp} list-commands.bat "$InstDirRedirect\site"
  !insertmacro APPEND_CONFIG ${Comp} emacs-init.el "$InstDirRedirect\site\.emacs.d\${STEM}"
  !insertmacro APPEND_CONFIG ${Comp} set-environment.el "$InstDirRedirect\site\.emacs.d\${STEM}"
  !insertmacro APPEND_CONFIG ${Comp} set-environment.bat "$InstDirRedirect\bin\lispx"
  GoTo ip_go_out
  
ip_report_already_installed:  
  
  DetailPrint "Already installed"
  GoTo already_installed
  
ip_go_out:  
!macroend

Section /o "Desktop shortcuts" SEC_Shortcuts
    !insertmacro REG_STR HKLM "${REGKEY}\Components" Shortcuts 1
SectionEnd

!macro CREATE_EMACS_SHORTCUT Comp Title
  #!insertmacro SET_OUTPUT_PATH "$SMPROGRAMS\$StartMenuGroup"
  CreateDirectory "$SMPROGRAMS\$StartMenuGroup"

  SetDetailsPrint none
  CreateShortcut "$SMPROGRAMS\$StartMenuGroup\${Title}.lnk" \
                 "$INSTDIR\${STEM}.exe" \
                 "/${Comp}" \
                 "$INSTDIR\images\emacs${_SUFFIX}.ico"
  SetDetailsPrint both          
             
  Push $0
  SectionGetFlags ${SEC_Shortcuts} $0
  IntOp $0 $0 & ${SF_SELECTED} 
  ${If} $0 != 0
#    !insertmacro SET_OUTPUT_PATH "$DESKTOP"
    SetDetailsPrint none
    CreateShortcut "$DESKTOP\${Title}.lnk" \
                   "$INSTDIR\${STEM}.exe" \
                   "/${Comp}" \
                   "$INSTDIR\images\emacs${_SUFFIX}.ico"
    SetDetailsPrint both
  ${EndIf}
  Pop $0
!macroend

!macro DELETE_EMACS_SHORTCUT Title
  Delete /REBOOTOK "$SMPROGRAMS\$StartMenuGroup\${Title}.lnk"
  Delete /REBOOTOK "$DESKTOP\${Title}.lnk"
!macroend

!macro UNINSTALL_PACKAGE Comp
  SetShellVarContext all
!macroend

/*!macro ENSURE_HOME_PATH
  ReadRegStr $0 ${ENV_HKCU} HOME
   
  ${If} $0 == ""
    ReadRegStr $0 ${ENV_HKLM} HOME
  ${EndIf}   
    
  ${If} $0 == ""
    ReadRegStr $0 HKCU "Software\Microsoft\Windows\CurrentVersion \
                        \Explorer\Shell Folders" Personal
    StrCpy $0 "$0\${STEM}Home"
  ${EndIf}   

   StrCpy $HomePath $0
!macroend*/

# Sections
Section /o -UninstallCurrent SEC_UNINSTALL
  !insertmacro CHECK_EXECUTION_STATE "${S_DOWNLOAD_FAILED}"
  
  DetailPrint "${S_REMOVING_CURRENT_VER}"
 
  SetDetailsPrint none
  ExecWait '"$InstalledVersionPath\uninstall.exe" /S _?=$InstalledVersionPath'
  RmDir /r $1
  SetDetailsPrint both
SectionEnd

Section -pre SEC_PRE
  ${If} $PrivateEnvironment == "sandbox"
  ${OrIf} $PrivateEnvironment == "portable"
    !insertmacro SET_OUTPUT_PATH "$InstDirRedirect"
    SetDetailsPrint none 
    File .\sandbox
    SetDetailsPrint both
    
    ${If} $CurrentSandbox != ""
    ${OrIf} $PrivateEnvironment == "portable"
      ClearErrors
      FileOpen $0 "$InstDirRedirect\sandbox" w
      ${If} $PrivateEnvironment == "sandbox"
        FileWrite $0 $CurrentSandbox
        CreateDirectory $CurrentSandbox
      ${Else}
        FileWrite $0 "../home"
        CreateDirectory "$INSTDIR\home"
      ${EndIf}
      FileClose $0
    ${EndIf}
  ${Else}
    SetDetailsPrint none 
    Delete "$INSTDIR\sandbox"
    SetDetailsPrint both
  ${EndIf}

#  StrCmp $PrivateEnvironment portable go_out

#  DetailPrint "${S_CREATE_RESTORE_POINT}"
#  SysRestore::StartRestorePoint /NOUNLOAD "Installed $(^Name)"

#go_out:
SectionEnd

SectionGroup "Rho Emacs"

Section -SHELL SEC_SHELL
SectionEnd

!macro INSTALL_EMACS Comp Size
!ifndef BASIC_EMACS
    !insertmacro CHECK_EXECUTION_STATE "${S_DOWNLOAD_FAILED}"
!endif

    !insertmacro SET_OUTPUT_PATH "$InstDirRedirect"
    SetDetailsPrint none
    File .\COPYING
    SetDetailsPrint both

    !insertmacro INSTALL_PACKAGE "${Comp}" ${Size}

    StrCmp $PrivateEnvironment portable already_installed
    
    SetDetailsPrint none
    ${EnvVarUpdate} $0 "PATH" "A" "HKLM" "$INSTDIR"
    SetDetailsPrint both

    !insertmacro CREATE_EMACS_SHORTCUT "${Comp}" "Rho Emacs"
#    !insertmacro CREATE_EMACS_SHORTCUT SHELL "Emacs (Command shell)"  

!ifndef BASIC_EMACS
	${RegisterExtension} "$INSTDIR\${STEM}.exe" "/TARGET:LISPX" \
      "$INSTDIR\bin\lispx\lispx-gui.exe,0" ".lispx" "${S_LISPX_SCRIPT}"
	${RegisterExtension} "$INSTDIR\${STEM}.exe" "/TARGET:LISPX" \
      "$INSTDIR\bin\lispx\lispx-gui.exe,1" ".lispxz" "${S_LISPX_DISTRIBUTION}"
!endif

    !insertmacro REG_STR HKLM "${REGKEY}\Components" "${Comp}" 1
already_installed:
!macroend


!ifdef BASIC_EMACS
!ifdef ARCH_64
Section "-Emacs 26.1 (x64)" SEC_emacs-x64
!else
Section /o "-Emacs 26.1 (x64)" SEC_emacs-x64
!endif
!else
Section "Emacs 26.1 (x64)" SEC_emacs-x64
!endif

!ifdef BASIC_EMACS
!ifdef ARCH_64
    !insertmacro SET_OUTPUT_PATH "$InstDirRedirect"

    File lite

    SetCompress off
    File ".\build\${STEM}-emacs-x64-${VERSION}.${ARC_EXT}"
    SetCompress auto
!endif
!endif

!ifdef BASIC_EMACS
    !insertmacro INSTALL_EMACS emacs-x64 450000
!else
    !insertmacro INSTALL_EMACS emacs-x64 521000
!endif
SectionEnd           


!ifdef BASIC_EMACS
!ifdef ARCH_32
Section "-Emacs 26.1 (x86)" SEC_emacs-x86
!else
Section /o "-Emacs 26.1 (x86)" SEC_emacs-x86
!endif
!else
Section /o "Emacs 26.1 (x86)" SEC_emacs-x86
!endif

!ifdef BASIC_EMACS
!ifdef ARCH_32
    !insertmacro SET_OUTPUT_PATH "$InstDirRedirect"

    File lite

    SetCompress off
    File ".\build\${STEM}-emacs-x86-${VERSION}.${ARC_EXT}"
    SetCompress auto
!endif
!endif

!ifdef BASIC_EMACS
    !insertmacro INSTALL_EMACS emacs-x86 600000
!else
    !insertmacro INSTALL_EMACS emacs-x86 700000
!endif
SectionEnd

Section "Basic enhancements" SEC_ConvKit
!ifdef BASIC_EMACS
    !insertmacro SET_OUTPUT_PATH "$InstDirRedirect"
    SetCompress off
    File /r ".\build\${STEM}-ConvKit-${VERSION}.${ARC_EXT}"
    SetCompress auto
!endif
    !insertmacro INSTALL_PACKAGE ConvKit 7000

    SetOutPath "$InstDirRedirect\fonts"
    ExecWait '"$InstDirRedirect\bin\utils\FontReg.exe" /copy'

    !insertmacro REG_STR HKLM "${REGKEY}\Components" "Basic enhancements" 1
already_installed:
SectionEnd

#Section /o "Auto Complete Mode" SEC_AutoComplete
#    !insertmacro APPEND_CONFIG AutoComplete emacs-init.el "$InstDirRedirect\site\.emacs.d\${STEM}"

#    !insertmacro REG_STR HKLM "${REGKEY}\Components" "AutoComplete Mode" 1
#SectionEnd

Section /o "-Advanced enhancements" SEC_AdditionalModes    
    !insertmacro APPEND_CONFIG AdditionalModes emacs-init.el "$InstDirRedirect\site\.emacs.d\${STEM}"

    !insertmacro REG_STR HKLM "${REGKEY}\Components" "Advanced enhancements" 1

    ${If} $PrivateEnvironment == portable
    
        IfFileExists $InstDirRedirect/../home/.emacs.d/elpa/archives install_adv_emacs_end1

        CreateDirectory "$InstDirRedirect\..\home\.emacs.d\elpa\"
        CopyFiles /SILENT "$InstDirRedirect\bin\emacs\thirdparty\*" "$InstDirRedirect\..\home\.emacs.d\elpa\"
        install_adv_emacs_end1:
    ${Else}
        IfFileExists $CurrentSandbox/.emacs.d/elpa/archives install_adv_emacs_end2

        CreateDirectory "$CurrentSandbox\.emacs.d\elpa\"
        CopyFiles /SILENT "$InstDirRedirect\bin\emacs\thirdparty\*" "$CurrentSandbox\.emacs.d\elpa\"
        install_adv_emacs_end2:
    ${EndIf}        

SectionEnd

Section /o "Spacemacs look" SEC_SpacemacsLook
    !insertmacro APPEND_CONFIG SpacemacsLook emacs-init.el "$InstDirRedirect\site\.emacs.d\${STEM}"
SectionEnd

Section /o "Spacemacs" SEC_spacemacs
!ifdef BASIC_EMACS
    !insertmacro SET_OUTPUT_PATH "$InstDirRedirect"
    SetCompress off
    File /r ".\build\${STEM}-spacemacs-${VERSION}.${ARC_EXT}"
    SetCompress auto
!endif

    !insertmacro INSTALL_PACKAGE spacemacs 10000

    !insertmacro APPEND_CONFIG spacemacs emacs-init.el "$InstDirRedirect\site\.emacs.d\${STEM}"

    !insertmacro REG_STR HKLM "${REGKEY}\Components" "Spacemacs" 1

    ${If} $PrivateEnvironment == portable
        CreateDirectory "$InstDirRedirect\..\home\.emacs.d\"
        CopyFiles /SILENT "$InstDirRedirect\bin\emacs\spacemacs\*" "$InstDirRedirect\..\home\.emacs.d\"
    ${Else}
        CreateDirectory "$CurrentSandbox\.emacs.d\"
        CopyFiles /SILENT "$InstDirRedirect\bin\emacs\spacemacs\*" "$CurrentSandbox\.emacs.d\"
    ${EndIf}        
already_installed:
SectionEnd

Section /o "Store backups in one place" SEC_CollectBackups
    !insertmacro APPEND_CONFIG CollectBackups emacs-init.el "$InstDirRedirect\site\.emacs.d\${STEM}"
SectionEnd

Section /o "Use Windows keybindings" SEC_CUA
    !insertmacro APPEND_CONFIG UseWindowsKeybindings emacs-init.el "$InstDirRedirect\site\.emacs.d\${STEM}"
SectionEnd

Section "-LoadCustom" SEC_LoadCustom
    !insertmacro APPEND_CONFIG LoadCustom emacs-init.el "$InstDirRedirect\site\.emacs.d\${STEM}"
    Delete "$InstDirRedirect\site\.emacs.d\${STEM}\$$*"
SectionEnd



SectionGroupEnd

!ifndef BASIC_EMACS

Section /o "MSYS2" SEC_MSYS
    !insertmacro INSTALL_PACKAGE MSYS 190000

    ClearErrors
    FileOpen $1 "$InstDirRedirect\bin\msys\etc\fstab" w
    FileWrite $1 "none / cygdrive binary,posix=0,noacl,user 0 0$\n"
#    FileWrite $1 "$InstDirRedirect/bin/mingw /mingw32$\n"
    FileWrite $1 "$CurrentSandbox/.msys/tmp /tmp$\n"
    FileWrite $1 "$CurrentSandbox/.msys /home"
    FileClose $1
    
    StrCmp $PrivateEnvironment portable already_installed
        
    !insertmacro REG_STR HKLM "${REGKEY}\Components" "MSYS" 1
already_installed:
SectionEnd

Section -Slime SEC_Slime
    ${If} $InstallSlime == y
        !insertmacro INSTALL_PACKAGE Slime 12000

        StrCmp $PrivateEnvironment portable already_installed

        !insertmacro REG_STR HKLM "${REGKEY}\Components" Slime 1
    ${EndIf}
already_installed:
SectionEnd

Section /o Clojure SEC_Clojure
    !insertmacro INSTALL_PACKAGE Clojure 68000
    
    StrCmp $PrivateEnvironment portable already_installed
    
    !insertmacro REG_STR HKLM "${REGKEY}\Components" Clojure 1
already_installed:
SectionEnd

SectionGroup "Common Lisp"

Section /o HyperSpec SEC_HyperSpec
    !insertmacro INSTALL_PACKAGE HyperSpec 14500

    !insertmacro SET_OUTPUT_PATH "$InstDirRedirect\docs"
    Nsis7z::Extract "$InstDirRedirect\docs\HyperSpec.7z"
    Delete "$InstDirRedirect\docs\HyperSpec.7z"

    StrCmp $PrivateEnvironment portable already_installed
    
#    !insertmacro SET_OUTPUT_PATH "$SMPROGRAMS\$StartMenuGroup\Documentation\Common Lisp"

#    SetDetailsPrint none
#    CreateShortcut "HyperSpec.lnk" \
#                   "$INSTDIR\docs\HyperSpec\Front\index.htm"
#    SetDetailsPrint both
    
    !insertmacro REG_STR HKLM "${REGKEY}\Components" HyperSpec 1
already_installed:
SectionEnd

Section /o "Clozure CL" SEC_ClozureCL
    !insertmacro INSTALL_PACKAGE ClozureCL 100000
    
    StrCmp $PrivateEnvironment portable already_installed
    
    !insertmacro REG_STR HKLM "${REGKEY}\Components" "Clozure CL" 1
already_installed:
SectionEnd

Section /o "SBCL" SEC_SBCL
    !insertmacro INSTALL_PACKAGE SBCL 33000
    
    StrCmp $PrivateEnvironment portable already_installed
    
    !insertmacro REG_STR HKLM "${REGKEY}\Components" "SBCL" 1
already_installed:
SectionEnd

Section /o "CLISP" SEC_clisp
    !insertmacro INSTALL_PACKAGE clisp 29000
    
    StrCmp $PrivateEnvironment portable already_installed
    
    !insertmacro REG_STR HKLM "${REGKEY}\Components" "clisp" 1
already_installed:
SectionEnd

Section /o "ABCL" SEC_ABCL
    !insertmacro INSTALL_PACKAGE ABCL 11000
    
    StrCmp $PrivateEnvironment portable already_installed
          
    !insertmacro REG_STR HKLM "${REGKEY}\Components" "ABCL" 1
already_installed:
SectionEnd


Section /o "ECL" SEC_ECL
    !insertmacro INSTALL_PACKAGE ECL 10000
    
    StrCmp $PrivateEnvironment portable already_installed
        
    !insertmacro REG_STR HKLM "${REGKEY}\Components" "ECL" 1
already_installed:
SectionEnd

#Section /o "Extra Libraries" SEC_LispLibraries
#    !insertmacro INSTALL_PACKAGE LispLibraries 25000
#    
#    StrCmp $PrivateEnvironment portable already_installed
#        
#    !insertmacro REG_STR HKLM "${REGKEY}\Components" "Extra Libraries" 1
#already_installed:
#SectionEnd

SectionGroupEnd

Section /o Racket SEC_MzScheme
    !insertmacro INSTALL_PACKAGE MzScheme 445000
#    !insertmacro APPEND_CONFIG MzScheme SetEnvVars.bat "$INSTDIR\run"
    
    StrCmp $PrivateEnvironment portable already_installed
    
#    !insertmacro SET_OUTPUT_PATH "$SMPROGRAMS\$StartMenuGroup\Documentation\Racket"

#    SetDetailsPrint none
#    CreateShortcut "R6RS Reference.lnk" \
#                   "$INSTDIR\docs\Racket\r6rs-std\index.html"
#    CreateShortcut "R6RS Library Reference.lnk" \
#                   "$INSTDIR\docs\Racket\r6rs-lib-std\index.html"
#    CreateShortcut "Racket Language Reference.lnk" \
#                   "$INSTDIR\docs\Racket\reference\index.html"
#    CreateShortcut "Racket Documentation (Web).lnk" \
#                   "http://docs.racket-lang.org/"   
#    SetDetailsPrint both
    
    !insertmacro REG_STR HKLM "${REGKEY}\Components" MzScheme 1
already_installed:
SectionEnd

#Section /o Shen SEC_Shen
#    !insertmacro INSTALL_PACKAGE Shen 31000
    
#    StrCmp $PrivateEnvironment portable already_installed
        
#    !insertmacro CREATE_EMACS_SHORTCUT Shen "Emacs (Shen)"
       
#    !insertmacro REG_STR HKLM "${REGKEY}\Components" Shen 1
#already_installed:
#SectionEnd


#SectionGroup "Extras" SEC_extras

SectionGroup "Python"

Section /o "Python 2.7" SEC_python2
    !insertmacro INSTALL_PACKAGE python2 76000
    
    StrCmp $PrivateEnvironment portable already_installed
         
    !insertmacro REG_STR HKLM "${REGKEY}\Components" "Python 2.7" 1
already_installed:
SectionEnd

Section /o "Python 3.2" SEC_python3
    !insertmacro INSTALL_PACKAGE python3 68000
    
    StrCmp $PrivateEnvironment portable already_installed
           
    !insertmacro REG_STR HKLM "${REGKEY}\Components" "Python 3.2" 1
already_installed:
SectionEnd

SectionGroupEnd

#SectionGroupEnd

Section /o "Groovy" SEC_groovy
    SetRegView 32
    WriteRegStr HKLM "SOFTWARE\JavaSoft\Prefs" ".nop" ""

    SetRegView 64
    WriteRegStr HKLM "SOFTWARE\JavaSoft\Prefs" ".nop" ""
 

    !insertmacro INSTALL_PACKAGE groovy 150000
    
    StrCmp $PrivateEnvironment portable already_installed
   
    !insertmacro REG_STR HKLM "${REGKEY}\Components" "Groovy" 1
already_installed:
SectionEnd


SectionGroup "Java"

Section /o "JDK" SEC_jdk
    !insertmacro INSTALL_PACKAGE jdk 226000
    
    StrCmp $PrivateEnvironment portable already_installed
    
    !insertmacro REG_STR HKLM "${REGKEY}\Components" "JDK" 1
already_installed:
SectionEnd

Section /o "Apache Ant" SEC_ant
    !insertmacro INSTALL_PACKAGE ant 37000
    
    StrCmp $PrivateEnvironment portable already_installed
    
    !insertmacro REG_STR HKLM "${REGKEY}\Components" "ant" 1
already_installed:
SectionEnd

Section /o "Apache Maven" SEC_maven
    !insertmacro INSTALL_PACKAGE maven 10000
    
    StrCmp $PrivateEnvironment portable already_installed
    
    !insertmacro REG_STR HKLM "${REGKEY}\Components" "maven" 1
already_installed:
SectionEnd

Section /o "Gradle" SEC_gradle
    !insertmacro INSTALL_PACKAGE gradle 73000
    
    StrCmp $PrivateEnvironment portable already_installed
    
    !insertmacro REG_STR HKLM "${REGKEY}\Components" "gradle" 1
already_installed:
SectionEnd


SectionGroupEnd


SectionGroup "C++"

Section /o "MinGW32" SEC_mingw32
    AddSize 1500000
    ExecWait '"$InstDirRedirect\bin\msys\msys2.exe" $InstDirRedirect/bin/msys/usr/bin/bash.exe -l -c "pacman --noconfirm -Sy --needed base-devel mingw-w64-i686-toolchain git subversion mercurial mingw-w64-i686-cmake mingw-w64-i686-boost"'
    #Exec '"$InstDirRedirect/batch/installmingw32u.cmd" $InstDirRedirect\bin\msys\'
    !insertmacro REG_STR HKLM "${REGKEY}\Components" "mingw32" 1
SectionEnd

Section /o "MinGW64" SEC_mingw64
    AddSize 1500000
    ExecWait '"$InstDirRedirect\bin\msys\msys2.exe" $InstDirRedirect/bin/msys/usr/bin/bash.exe -l -c "pacman -Sy --noconfirm --needed base-devel mingw-w64-x86_64-toolchain git subversion mercurial mingw-w64-x86_64-cmake mingw-w64-x86_64-boost"'
    !insertmacro REG_STR HKLM "${REGKEY}\Components" "mingw64" 1
SectionEnd


SectionGroupEnd

!endif # BASIC_EMACS


Section -post SEC_post
    SetDetailsPrint none

    RmDir /r "$InstDirRedirect\scripts"

    StrCmp $PrivateEnvironment portable continue_portable

    SetShellVarContext all
    
#    SetDetailsPrint none
#    !insertmacro MoveFolder "$INSTDIR\~home" "$HomePath" "*"
#    SetDetailsPrint both
     
    WriteUninstaller $INSTDIR\uninstall.exe
   
    !insertmacro SET_OUTPUT_PATH "$SMPROGRAMS\$StartMenuGroup"

    SetDetailsPrint none
    CreateShortcut "Uninstall $(^Name).lnk" "$INSTDIR\uninstall.exe"
    SetDetailsPrint both
                                     
    !insertmacro REG_STR HKLM "${REGKEY}" Path $INSTDIR
    !insertmacro REG_STR HKLM "${REGKEY}" Version "${VERSION}"
    !insertmacro REG_STR HKLM "SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\$(^Name)" DisplayName "$(^Name)"
    !insertmacro REG_STR HKLM "SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\$(^Name)" DisplayVersion "${VERSION}"
#    !insertmacro REG_STR HKLM "SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\$(^Name)" URLInfoAbout "${URL}"
    !insertmacro REG_STR HKLM "SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\$(^Name)" DisplayIcon $INSTDIR\uninstall.exe
    !insertmacro REG_STR HKLM "SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\$(^Name)" UninstallString $INSTDIR\uninstall.exe
    WriteRegDWORD HKLM "SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\$(^Name)" NoModify 1
    WriteRegDWORD HKLM "SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\$(^Name)" NoRepair 1

continue_portable:
    ${If} $PrivateEnvironment == "portable"
      CopyFiles /SILENT "$InstDirRedirect\${STEM}.exe" "$INSTDIR\"
      Rename "$InstDirRedirect\${STEM}.conf.portable" \
             "$INSTDIR\${STEM}.conf"

#      ${If} ${SectionIsSelected} ${SEC_Clojure}
#      ${OrIf} ${SectionIsSelected} ${SEC_ABCL}
#        Call PortableInstallJRE
#      ${EndIf}
    ${Else}
        Delete "$InstDirRedirect\${STEM}.conf.portable"
    ${EndIf}

    SetDetailsPrint both
    DetailPrint "Initializing the installation..."
    SetDetailsPrint none
#    ${If} ${SectionIsSelected} ${SEC_python2}
#    ${OrIf} ${SectionIsSelected} ${SEC_python3}
#      ExecWait '"$InstDirRedirect\bin\utils\vcredist_x86.exe" /q'  
#    ${EndIf}

    ExecWait '"$InstDirRedirect\${STEM}.exe" /TARGET:PRECOMP /WAIT'

    SetDetailsPrint both
    DetailPrint "Initialization finished"
    SetDetailsPrint none

#    SysRestore::FinishRestorePoint
SectionEnd

# Macro for selecting uninstaller sections
!macro SELECT_UNSECTION SECTION_NAME UNSECTION_ID
    Push $R0
    ReadRegStr $R0 HKLM "${REGKEY}\Components" "${SECTION_NAME}"
    StrCmp $R0 1 0 next${UNSECTION_ID}
    !insertmacro SelectSection "${UNSECTION_ID}"
    GoTo done${UNSECTION_ID}
next${UNSECTION_ID}:
    !insertmacro UnselectSection "${UNSECTION_ID}"
done${UNSECTION_ID}:
    Pop $R0
!macroend

Section -un.post UNSEC_post
    SetShellVarContext all

    ${un.EnvVarUpdate} $0 "PATH" "R" "HKLM" "$INSTDIR"
!ifndef BASIC_EMACS
	${UnregisterExtension} ".lispx" "${S_LISPX_SCRIPT}"
	${UnregisterExtension} ".lispxz" "${S_LISPX_DISTRIBUTION}"
!endif    

    DeleteRegKey HKLM "SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\$(^Name)"
    DeleteRegValue HKLM "${REGKEY}" Path
    DeleteRegKey HKLM "${REGKEY}\Components"
    DeleteRegKey HKLM "${REGKEY}"
    RmDir /r /REBOOTOK $SMPROGRAMS\$StartMenuGroup
    RmDir /r /REBOOTOK $INSTDIR

#    SysRestore::FinishRestorePoint
SectionEnd

# Page custom handlers

Function DetectInstalledVersion
  Push $0
  
  ReadRegStr $0 HKLM "${REGKEY}" Version
  ReadRegStr $InstalledVersionPath HKLM "${REGKEY}" Path    
  
  IfFileExists "$InstalledVersionPath\sandbox" 0 proceed
  ClearErrors
  FileOpen $0 "$InstalledVersionPath\sandbox" r
  FileRead $0 $CurrentSandbox
  FileClose $0
    
  ${Trim} $CurrentSandbox $CurrentSandbox
  
proceed:
  
  ${If} "" != $InstalledVersionPath
  ${AndIf} $0 != "${VERSION}"
	StrCpy $UninstallPreviousVersion t
  ${EndIf}
  
#  GoTo go_out
#proceed:  
  
#  SectionGetFlags ${SEC_UNINSTALL} $0
#  IntOp $0 $0 | ${SF_SELECTED}
#  SectionSetFlags ${SEC_UNINSTALL} $0

#GoTo go_out  
  
#exit:
#  Quit
  
#go_out:
  Pop $0
FunctionEnd

Function leaveWelcome
  Call DetectInstalledVersion
FunctionEnd

Function leaveComponents

FunctionEnd
 
Function CustomSettings
  !insertmacro MUI_HEADER_TEXT "${S_SETTINGS_TEXT}" "${S_SETTINGS_SUBTEXT}"
  ReserveFile "setup.ini"
  !insertmacro INSTALLOPTIONS_EXTRACT "setup.ini"
  
  ${If} $CurrentSandbox != ""
    !insertmacro INSTALLOPTIONS_WRITE "setup.ini" "Field 5" "State" $CurrentSandbox
  ${Else}
    ReadRegStr $0 HKCU \
             "SOFTWARE\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders" \
             Personal
    !insertmacro INSTALLOPTIONS_WRITE "setup.ini" "Field 5" "State" "$0\rhome"
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
  !insertmacro INSTALLOPTIONS_READ $CurrentSandbox "setup.ini" "Field 5" "State"
  
  ${If} $0 == 1
  
!ifndef BASIC_EMACS
    StrLen $2 $CurrentSandbox
    IntOp $2 $2 - 1
    ${ForEach} $1 0 $2 + 1
      StrCpy $3 $currentSandbox 1 $1
      ${If} $3 == " "
        MessageBox MB_YESNO|MB_ICONEXCLAMATION "${S_WARN_SPACES}" IDYES resume_installation
        Abort
      ${EndIf}
    ${Next}
!endif
    
resume_installation:

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

  StrCpy $INSTDIR "$PROFILE\rho"
  StrCmp $CommandParams "/TEST" leave_custom_debug leave_custom_finish

leave_custom_debug:

StrCpy $INSTDIR "D:\tmp\rho" 

leave_custom_finish:
FunctionEnd

Function leaveDirectory
  
  StrCpy $0 "$%PROGRAMFILES%"
  ${StrStr} $1 $INSTDIR $0
  ${If} $1 != "" 
    MessageBox MB_YESNO|MB_ICONQUESTION "${S_SUSPICIOUS_INSTDIR}" IDYES check_path \
                                                                  IDNO proceed
proceed:
	Abort          
  ${EndIf}

check_path:

!ifndef BASIC_EMACS
  ${If} ${SectionIsSelected} ${SEC_Clojure}
    StrLen $2 $INSTDIR
    IntOp $2 $2 - 1
    ${ForEach} $1 0 $2 + 1
      StrCpy $3 $INSTDIR 1 $1
      ${If} $3 == " "
        MessageBox MB_YESNO|MB_ICONEXCLAMATION "${S_WARN_SPACES_INST}" IDYES go_out
        Abort
      ${EndIf}
    ${Next}
  ${EndIf}
!endif

go_out:
FunctionEnd

Function DownloadComponents
!ifndef BASIC_EMACS
  !insertmacro LOAD_PACKAGE emacs-x64
  !insertmacro LOAD_PACKAGE emacs-x86
  !insertmacro LOAD_PACKAGE ConvKit
  !insertmacro LOAD_PACKAGE spacemacs
  !insertmacro LOAD_PACKAGE MSYS
  !insertmacro LOAD_PACKAGE jdk
  !insertmacro LOAD_PACKAGE ant
  !insertmacro LOAD_PACKAGE maven
  !insertmacro LOAD_PACKAGE gradle  

  !insertmacro LOAD_PACKAGE HyperSpec
  !insertmacro LOAD_PACKAGE ClozureCL
  !insertmacro LOAD_PACKAGE SBCL
  !insertmacro LOAD_PACKAGE CLISP
  !insertmacro LOAD_PACKAGE ABCL
  !insertmacro LOAD_PACKAGE ECL
#  !insertmacro LOAD_PACKAGE LispLibraries
  !insertmacro LOAD_PACKAGE Clojure
  !insertmacro LOAD_PACKAGE MzScheme
#  !insertmacro LOAD_PACKAGE Shen 

  !insertmacro LOAD_PACKAGE python2
  !insertmacro LOAD_PACKAGE python3
  !insertmacro LOAD_PACKAGE groovy  
#  !insertmacro LOAD_PACKAGE mingw
#  !insertmacro LOAD_PACKAGE boost


  
  ${If} ${SectionIsSelected} ${SEC_ClozureCL}
  ${OrIf} ${SectionIsSelected} ${SEC_SBCL}
  ${OrIf} ${SectionIsSelected} ${SEC_ABCL}
  ${OrIf} ${SectionIsSelected} ${SEC_ECL}
  ${OrIf} ${SectionIsSelected} ${SEC_CLISP}
  ${OrIf} ${SectionIsSelected} ${SEC_Clojure}
    StrCpy $InstallSlime y
    !insertmacro LOAD_PACKAGE Slime
  ${Else}
    StrCpy $InstallSlime n
  ${EndIf}
!endif
FunctionEnd

Function showInstFiles
#  !insertmacro ENSURE_HOME_PATH
  
  ${If} $PrivateEnvironment == portable
    StrCpy $InstDirRedirect "$INSTDIR\${STEM}"
  ${Else}
    StrCpy $InstDirRedirect $INSTDIR
  ${EndIf}
  
  !insertmacro SET_OUTPUT_PATH "$InstDirRedirect"

  StrCpy $ExecutionState S_OK

  Call DownloadComponents
FunctionEnd

Function un.showInstFiles
#  !insertmacro ENSURE_HOME_PATH
FunctionEnd

# Installer functions
Function .onInit
    InitPluginsDir
    StrCpy $StartMenuGroup "$(^Name)"
#    SectionSetFlags ${SEC_EmacsW32} 17
    
    StrCpy $switch_overwrite 1
!ifndef BASIC_EMACS
    StrCpy $DoCheckForRecentVersion y
!endif
    ${GetParameters} $CommandParams

#    SectionSetFlags ${SEC_emacs-x64} 17
    !insertmacro RadioGetChecked "${SEC_emacs-x86}" "${SEC_emacs-x86},${SEC_emacs-x64}"
FunctionEnd

Function .onSelChange
    !insertmacro SectionRadioButtons "${SEC_emacs-x86}" "${SEC_emacs-x86},${SEC_emacs-x64}"
!ifndef BASIC_EMACS
    ${If} ${SectionIsSelected} ${SEC_mingw32}
    ${OrIf} ${SectionIsSelected} ${SEC_mingw64}
        !insertmacro SetSectionFlag ${SEC_MSYS} ${SF_RO}
        !insertmacro SelectSection ${SEC_MSYS}
    ${Else}
        !insertmacro ClearSectionFlag ${SEC_MSYS} ${SF_RO}
    ${EndIf}
!endif

    ${If} ${SectionIsSelected} ${SEC_AdditionalModes}
        !insertmacro SetSectionFlag ${SEC_ConvKit} ${SF_RO}
        !insertmacro SelectSection ${SEC_ConvKit}
    ${Else}
        !insertmacro ClearSectionFlag ${SEC_ConvKit} ${SF_RO}
    ${EndIf}

    ${If} ${SectionIsSelected} ${SEC_spacemacs}
        !insertmacro UnSelectSection ${SEC_ConvKit}
        !insertmacro UnSelectSection ${SEC_AdditionalModes}
        !insertmacro UnSelectSection ${SEC_SpacemacsLook}
        !insertmacro SetSectionFlag ${SEC_ConvKit} ${SF_RO}
        !insertmacro SetSectionFlag ${SEC_AdditionalModes} ${SF_RO}
        !insertmacro SetSectionFlag ${SEC_SpacemacsLook} ${SF_RO}
    ${Else}
        !insertmacro ClearSectionFlag ${SEC_ConvKit} ${SF_RO}
        !insertmacro ClearSectionFlag ${SEC_AdditionalModes} ${SF_RO}
        !insertmacro ClearSectionFlag ${SEC_SpacemacsLook} ${SF_RO}
    ${EndIf}
FunctionEnd

# Uninstaller functions
Function un.onInit
    ReadRegStr $INSTDIR HKLM "${REGKEY}" Path
    StrCpy $StartMenuGroup "$(^Name)"

#    SysRestore::StartUnRestorePoint /NOUNLOAD "Uninstalled $(^Name)"
FunctionEnd

# Section Descriptions
!insertmacro MUI_FUNCTION_DESCRIPTION_BEGIN
!insertmacro MUI_DESCRIPTION_TEXT ${SEC_emacs-x64} "Emacs 26.1 (64-bit) build for Microsoft Windows bundled with modules and EmacsW32 enchancement pack"
!insertmacro MUI_DESCRIPTION_TEXT ${SEC_emacs-x86} "Emacs 26.1 (32-bit) build for Microsoft Windows bundled with EmacsW32 enchancement pack"
!insertmacro MUI_DESCRIPTION_TEXT ${SEC_ConvKit} "Basic Emacs setup for convenient editing: arjen theme, ido, linum, column-marker, highlight-symbol, show-paren, save-place, org-wiki"
#!insertmacro MUI_DESCRIPTION_TEXT ${SEC_AutoComplete} "Global visual (popup menu) autocompletion mode for Emacs (available manually with the `auto-complete-mode' command if unchecked)"
!insertmacro MUI_DESCRIPTION_TEXT ${SEC_AdditionalModes} "A set of advanced modes: bookmark+, yasnippet, helm, sr-speedbar, window-purpose"
!insertmacro MUI_DESCRIPTION_TEXT ${SEC_SpacemacsLook} "Get Spacemacs look without installing Spacemacs"
!insertmacro MUI_DESCRIPTION_TEXT ${SEC_spacemacs} "Bring Vim experience to Emacs (only check this if you know what you are doing)"
!insertmacro MUI_DESCRIPTION_TEXT ${SEC_CollectBackups} "Store bacup files not near the edited file but in ~/emscs.d/backups"
!insertmacro MUI_DESCRIPTION_TEXT ${SEC_CUA} "Enable CUA mode"
!ifndef BASIC_EMACS
!insertmacro MUI_DESCRIPTION_TEXT ${SEC_HyperSpec} "Reference documentation for Common Lisp"
!insertmacro MUI_DESCRIPTION_TEXT ${SEC_ClozureCL} "Implementation of Clozure Common Lisp for Microsoft Windows"
!insertmacro MUI_DESCRIPTION_TEXT ${SEC_SBCL} "Implementation of Steel Bank Common Lisp for Microsoft Windows"
!insertmacro MUI_DESCRIPTION_TEXT ${SEC_ABCL} "Armed Bear Common Lisp Implementation for JVM"
!insertmacro MUI_DESCRIPTION_TEXT ${SEC_ECL} "Embeddable Common Lisp compiled with unicode and threading support (requires Microsoft Visual C++ compiler)"
#!insertmacro MUI_DESCRIPTION_TEXT ${SEC_LispLibraries} "Additional Lisp libraries for web development (including 'Weblocks' and 'UnCommon Web' web frameworks)"
!insertmacro MUI_DESCRIPTION_TEXT ${SEC_clisp} "CLISP for Microsoft Windows"
!insertmacro MUI_DESCRIPTION_TEXT ${SEC_Clojure} "Clojure Lisp"
!insertmacro MUI_DESCRIPTION_TEXT ${SEC_MzScheme} "Racket Scheme for Microsoft Windows"
#!insertmacro MUI_DESCRIPTION_TEXT ${SEC_Shen} "Shen programming language (former Qi)"
#!insertmacro MUI_DESCRIPTION_TEXT ${SEC_extras} "Additional tools and languages"
!insertmacro MUI_DESCRIPTION_TEXT ${SEC_MSYS} "A minimalist Bourne Shell port for Windows"
!insertmacro MUI_DESCRIPTION_TEXT ${SEC_python2} "Python 2.7"
!insertmacro MUI_DESCRIPTION_TEXT ${SEC_python3} "Python 3.7"
!insertmacro MUI_DESCRIPTION_TEXT ${SEC_mingw32} "Install the latest MinGW-x64 (32-bit) into MSYS2 directory"
!insertmacro MUI_DESCRIPTION_TEXT ${SEC_mingw64} "Install the latest MinGW-x64 (64-bit) into MSYS2 directory"
#!insertmacro MUI_DESCRIPTION_TEXT ${SEC_boost} "The Boost Library (1.50)"
!insertmacro MUI_DESCRIPTION_TEXT ${SEC_jdk} "Private instances of JDK and JRE v8.0 available through the environment of Rho Emacs (no system integration)"
!insertmacro MUI_DESCRIPTION_TEXT ${SEC_groovy} "Groovy 2 + Grails 3"
!endif
!insertmacro MUI_FUNCTION_DESCRIPTION_END

