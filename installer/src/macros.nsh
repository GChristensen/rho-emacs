# Shared macros
# Part of the Rho Emacs installer, included from installer\rho.nsi

!macro REG_STR root_key subkey key_name value
  ${If} $PrivateEnvironment != portable
    WriteRegStr ${root_key} "${subkey}" "${key_name}" "${value}"
  ${EndIf}
!macroend

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
