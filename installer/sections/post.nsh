# Final installation steps and the uninstaller
# Part of the Rho Emacs installer, included from installer\rho.nsi

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
