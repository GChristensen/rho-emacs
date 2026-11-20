# Shared functions
# Part of the Rho Emacs installer, included from installer\rho.nsi

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

Function MergeLocation_
  ${StrStrAdv} $R1 $R9 $R0 > > 0 0 0
  ${StrStrAdv} $R2 $R9 $R0 > < 0 0 0
  
  SetDetailsPrint none
  !insertmacro MoveFolder $R9 "$R2$R1" "*.*"
  SetDetailsPrint both
  Push $R0
FunctionEnd

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
