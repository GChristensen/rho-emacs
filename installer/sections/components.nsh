# Optional components: ConvKit, Tabbar and Org tools
# Part of the Rho Emacs installer, included from installer\rho.nsi

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
