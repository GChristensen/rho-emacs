# Installer and uninstaller initialization
# Part of the Rho Emacs installer, included from installer\rho.nsi

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
