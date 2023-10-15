# Rho Emacs installer                    
# (C) 2021 g/christensen

Unicode True

Name "Rho Emacs"
!define STEM "rho"
!define _SUFFIX ""
!define COPYRIGHT "(C) 2022 g/christensen"


SetCompressor lzma
RequestExecutionLevel admin

# General Symbol Definitions
!define VERSION 1.2.2
!define VERSION_SUFFIX ${VERSION}
!define REGKEY "SOFTWARE\$(^Name)"
BrandingText "$(^Name) v${VERSION}"

# MUI Symbol Definitions
!define MUI_ICON "${NSISDIR}\Contrib\Graphics\Icons\modern-install-blue.ico"
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

!define MUI_HEADERIMAGE
!define MUI_HEADERIMAGE_BITMAP ".\nsis\images\header.bmp"

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

!macro REG_STR root_key subkey key_name value
  ${If} $PrivateEnvironment != portable
    WriteRegStr ${root_key} "${subkey}" "${key_name}" "${value}"
  ${EndIf}
!macroend

function StrReplace
  Exch $0 ;this will replace wrong characters
  Exch
  Exch $1 ;needs to be replaced
  Exch
  Exch 2
  Exch $2 ;the original string
  Push $3 ;counter
  Push $4 ;temp character
  Push $5 ;temp string
  Push $6 ;length of string that need to be replaced
  Push $7 ;length of string that will replace
  Push $R0 ;tempstring
  Push $R1 ;tempstring
  Push $R2 ;tempstring
  StrCpy $3 "-1"
  StrCpy $5 ""
  StrLen $6 $1
  StrLen $7 $0
  Loop:
  IntOp $3 $3 + 1
  Loop_noinc:
  StrCpy $4 $2 $6 $3
  StrCmp $4 "" ExitLoop
  StrCmp $4 $1 Replace
  Goto Loop
  Replace:
  StrCpy $R0 $2 $3
  IntOp $R2 $3 + $6
  StrCpy $R1 $2 "" $R2
  StrCpy $2 $R0$0$R1
  IntOp $3 $3 + $7
  Goto Loop_noinc
  ExitLoop:
  StrCpy $0 $2
  Pop $R2
  Pop $R1
  Pop $R0
  Pop $7
  Pop $6
  Pop $5
  Pop $4
  Pop $3
  Pop $2
  Pop $1
  Exch $0
FunctionEnd

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

  FileSeek  $0 0 END

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

Section /o "Desktop shortcuts" SEC_Shortcuts
    !insertmacro REG_STR HKLM "${REGKEY}\Components" Shortcuts 1
SectionEnd

!macro CREATE_EMACS_SHORTCUT Comp Title
  #!insertmacro SET_OUTPUT_PATH "$SMPROGRAMS\$StartMenuGroup"
  CreateDirectory "$SMPROGRAMS\$StartMenuGroup"

  SetDetailsPrint none
  CreateShortcut "$SMPROGRAMS\$StartMenuGroup\${Title}.lnk" \
                 "$INSTDIR\${STEM}.exe" \
                 "" \
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
                   "" \
                   "$INSTDIR\images\emacs${_SUFFIX}.ico"
    SetDetailsPrint both
  ${EndIf}
  Pop $0
!macroend

!macro DELETE_EMACS_SHORTCUT Title
  Delete /REBOOTOK "$SMPROGRAMS\$StartMenuGroup\${Title}.lnk"
  Delete /REBOOTOK "$DESKTOP\${Title}.lnk"
!macroend

!macro INSTALL_PACKAGE Comp Size
  AddSize ${Size}
  SetShellVarContext all

  !insertmacro APPEND_CONFIG ${Comp} ${STEM}.conf "$INSTDIR"
  !insertmacro APPEND_CONFIG ${Comp} emacs-init.el "$INSTDIR\lisp\${STEM}"
  !insertmacro APPEND_CONFIG ${Comp} set-environment.el "$INSTDIR\lisp\${STEM}"
!macroend

!macro UNINSTALL_PACKAGE Comp
  SetShellVarContext all
!macroend

function WriteConfig
    Pop $9
    StrCpy $5 ""
    StrCpy $6 ""

    ${If} $PrivateEnvironment == "sandbox"
      ${OrIf} $PrivateEnvironment == "portable"
        ClearErrors
        FileOpen $4 "$INSTDIR\${STEM}$9.conf" r
        ${Do}
           FileRead $4 $6
           ${If} ${Errors}
              ${ExitDo}
           ${EndIf}
           StrCpy $5 "$5$6"
        ${Loop}
        FileClose $4

        ${If} $SelectedHome != ""
        ${OrIf} $PrivateEnvironment == "portable"
          ClearErrors
          FileOpen $0 "$INSTDIR\${STEM}$9.conf" w
          ${If} $PrivateEnvironment == "sandbox"
            Push $5
            Push "HOME="
            Push "HOME=$SelectedHome"
            Call StrReplace
            Pop $1
            CreateDirectory $SelectedHome
          ${Else}
            Push $5
            Push "HOME="
            Push "HOME=home"
            Call StrReplace
            Pop $1
            CreateDirectory "$INSTDIR\home"
          ${EndIf}
          FileWrite $0 $1
          FileClose $0
        ${EndIf}
      ${EndIf}
FunctionEnd

ReserveFile "setup.ini"

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

Section "Basic enhancements" SEC_ConvKit
    SetOutPath "$INSTDIR\"
    File /r "packages\convkit\"
    File /r "elc\convkit\"

    !insertmacro INSTALL_PACKAGE ConvKit 0

    SetOutPath "$INSTDIR\fonts"
    ExecWait '"$INSTDIR\utils\FontReg.exe" /copy'
SectionEnd

Section "Tabbar" SEC_Tabbar
    SetOutPath "$INSTDIR\"
    File /r "packages\tabbar\"
    File /r "elc\tabbar\"

    !insertmacro INSTALL_PACKAGE tabbar 0
SectionEnd


SectionGroup "Org tools"
    Section /o "org-protocol" SEC_orgprotocol
        !insertmacro REG_STR HKCR "org-protocol" "" "URL:Org Protocol"
        !insertmacro REG_STR HKCR "org-protocol" "URL Protocol" ""
        !insertmacro REG_STR HKCR "org-protocol\shell\open\command" "" "$\"$INSTDIR\rho.exe$\" /TARGET:ORG_PROTOCOL $\"%1$\""

        !insertmacro REG_STR HKCR "ext+scrapyard" "" "URL:Scrapyard Protocol"
        !insertmacro REG_STR HKCR "ext+scrapyard" "URL Protocol" ""
        !insertmacro REG_STR HKCR "ext+scrapyard\shell\open\command" "" "$\"C:\Program Files\Mozilla Firefox\firefox.exe$\" $\"%1$\""
        !insertmacro APPEND_CONFIG orgprotocol emacs-init.el "$INSTDIR\lisp\${STEM}"
    SectionEnd

    Section /o "org-wiki" SEC_orgwiki
        !insertmacro APPEND_CONFIG orgwiki emacs-init.el "$INSTDIR\lisp\${STEM}"

        SetOutPath "$INSTDIR\java"
        File /r "software\java\"

        SetOutPath "$INSTDIR\"
        File /r "packages\orgwiki\"
        File /r "elc\orgwiki\"
    SectionEnd

    Section /o "org-roam" SEC_orgroam
        SetOutPath "$INSTDIR\"
        File /r "packages\orgroam\"

        !insertmacro APPEND_CONFIG orgroam emacs-init.el "$INSTDIR\lisp\${STEM}"
    SectionEnd
SectionGroupEnd


SectionGroup "Themes"

Section /o "No theme" SEC_ThemeNOP
SectionEnd

Section /o "Spacemacs dark" SEC_SpacemacsDark
    !insertmacro APPEND_CONFIG SpacemacsDark emacs-init.el "$INSTDIR\lisp\${STEM}"
SectionEnd

Section /o "Spacemacs light" SEC_SpacemacsLight
    !insertmacro APPEND_CONFIG SpacemacsLight emacs-init.el "$INSTDIR\lisp\${STEM}"
SectionEnd

Section /o "Moe dark" SEC_MoeDark
    !insertmacro APPEND_CONFIG MoeDark emacs-init.el "$INSTDIR\lisp\${STEM}"
SectionEnd

Section /o "Moe light" SEC_MoeLight
    !insertmacro APPEND_CONFIG MoeLight emacs-init.el "$INSTDIR\lisp\${STEM}"
SectionEnd

Section /o "Arjen" SEC_Arjen
    !insertmacro APPEND_CONFIG Arjen emacs-init.el "$INSTDIR\lisp\${STEM}"
SectionEnd

SectionGroupEnd

Section /o "Store backups in one place" SEC_CollectBackups
    !insertmacro APPEND_CONFIG CollectBackups emacs-init.el "$INSTDIR\lisp\${STEM}"
SectionEnd

Section /o "Use Windows keybindings" SEC_CUA
    !insertmacro APPEND_CONFIG UseWindowsKeybindings emacs-init.el "$INSTDIR\lisp\${STEM}"
SectionEnd

Section "-LoadCustom" SEC_LoadCustom
    !insertmacro APPEND_CONFIG LoadCustom emacs-init.el "$INSTDIR\lisp\${STEM}"
    Delete "$INSTDIR\lisp\${STEM}\$$*"
SectionEnd

Section -post SEC_post
    SetDetailsPrint none

    # !IMPORTANT: comment out for emacs 28
    ${If} $SelectedHome != ""
        Delete "$SelectedHome\.emacs.d\recentf"
    ${EndIf}

    StrCmp $PrivateEnvironment portable continue_portable

    SetShellVarContext all

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
    !insertmacro REG_STR HKLM "SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\$(^Name)" DisplayIcon $INSTDIR\images\emacs.ico
    !insertmacro REG_STR HKLM "SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\$(^Name)" UninstallString $INSTDIR\uninstall.exe
    WriteRegDWORD HKLM "SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\$(^Name)" NoModify 1
    WriteRegDWORD HKLM "SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\$(^Name)" NoRepair 1

continue_portable:

    #SetDetailsPrint both
    #DetailPrint "Compiling elisp libraries..."
    #SetDetailsPrint none

    #ExecWait '"$INSTDIR\${STEM}.exe" /TARGET:PRECOMP /SHOW /WAIT'

    SetDetailsPrint both
    #DetailPrint "Compilation finished"
    DetailPrint "Installation complete"
    SetDetailsPrint none
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

    EnVar::SetHKCU
    EnVar::DeleteValue "PATH" "$INSTDIR"
    Pop $0
    #${un.EnvVarUpdate} $0 "PATH" "R" "HKLM" "$INSTDIR"

    DeleteRegKey HKLM "SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\$(^Name)"
    DeleteRegValue HKLM "${REGKEY}" Path
    DeleteRegKey HKLM "${REGKEY}\Components"
    DeleteRegKey HKLM "${REGKEY}"

    !insertmacro DELETE_EMACS_SHORTCUT "Rho Emacs"

    RmDir /r /REBOOTOK $SMPROGRAMS\$StartMenuGroup
    RmDir /r /REBOOTOK $INSTDIR
SectionEnd

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
  !insertmacro INSTALLOPTIONS_EXTRACT "setup.ini"
  
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

# Installer functions
Function .onInit
    InitPluginsDir
    StrCpy $StartMenuGroup "$(^Name)"

    StrCpy $switch_overwrite 1
    ${GetParameters} $CommandParams

    !insertmacro SectionRadioButtons "${SEC_ThemeNOP}" "${SEC_ThemeNOP},${SEC_SpacemacsDark},${SEC_SpacemacsLight},${SEC_MoeDark},${SEC_MoeLight},${SEC_Arjen}"
#    SectionSetFlags ${SEC_emacs-x64} 17 ; mandatory sec
FunctionEnd

Function .onSelChange
    !insertmacro SectionRadioButtons "${SEC_ThemeNOP}" "${SEC_ThemeNOP},${SEC_SpacemacsDark},${SEC_SpacemacsLight},${SEC_MoeDark},${SEC_MoeLight},${SEC_Arjen}"
FunctionEnd

# Uninstaller functions
Function un.onInit
    ReadRegStr $INSTDIR HKLM "${REGKEY}" Path
    StrCpy $StartMenuGroup "$(^Name)"
FunctionEnd

# Section Descriptions
!insertmacro MUI_FUNCTION_DESCRIPTION_BEGIN
!insertmacro MUI_DESCRIPTION_TEXT ${SEC_Shortcuts} "Create desktop shortcuts"
!insertmacro MUI_DESCRIPTION_TEXT ${SEC_ConvKit} "Enables the following modes: ido, fido, iswithcb, ibuffer, uniquify, save-place, desktop-save, bookmark+, org-bullets"
!insertmacro MUI_DESCRIPTION_TEXT ${SEC_Tabbar} "Buffer grouping tab bar with mouse scroll support"
!insertmacro MUI_DESCRIPTION_TEXT ${SEC_orgprotocol} "Make org-protocol:// links to open in Rho Emacs"
!insertmacro MUI_DESCRIPTION_TEXT ${SEC_orgwiki} "Install and configure the org-wiki package"
!insertmacro MUI_DESCRIPTION_TEXT ${SEC_orgroam} "Install and configure the org-roam and org-roam-ui packages"
!insertmacro MUI_DESCRIPTION_TEXT ${SEC_ThemeNOP} "Do not activate any themes"
!insertmacro MUI_DESCRIPTION_TEXT ${SEC_SpacemacsDark} "Enables the selected theme by default"
!insertmacro MUI_DESCRIPTION_TEXT ${SEC_SpacemacsLight} "Enables the selected theme by default"
!insertmacro MUI_DESCRIPTION_TEXT ${SEC_MoeDark} "Enables the selected theme by default"
!insertmacro MUI_DESCRIPTION_TEXT ${SEC_MoeLight} "Enables the selected theme by default"
!insertmacro MUI_DESCRIPTION_TEXT ${SEC_Arjen} "Enables the selected theme by default"
!insertmacro MUI_DESCRIPTION_TEXT ${SEC_CollectBackups} "Store all backups in ~/emscs.d/backups"
!insertmacro MUI_DESCRIPTION_TEXT ${SEC_CUA} "Enable CUA mode"
!insertmacro MUI_FUNCTION_DESCRIPTION_END

