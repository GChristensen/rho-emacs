# Theme selection
# Part of the Rho Emacs installer, included from installer\rho.nsi

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
