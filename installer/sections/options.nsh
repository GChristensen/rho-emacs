# Optional behaviors
# Part of the Rho Emacs installer, included from installer\rho.nsi

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
