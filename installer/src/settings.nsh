# Product name, version and Modern UI symbols
# Part of the Rho Emacs installer, included from installer\rho.nsi

Name "Rho Emacs"
!define STEM "rho"
!define _SUFFIX ""
!define COPYRIGHT "(C) 2026 g/christensen"


SetCompressor lzma
RequestExecutionLevel admin

# General Symbol Definitions
!define VERSION 1.2.7
!define VERSION_SUFFIX ${VERSION}
!define REGKEY "SOFTWARE\$(^Name)"
BrandingText "$(^Name) v${VERSION}"

# MUI Symbol Definitions
!define MUI_ICON "${NSISDIR}\Contrib\Graphics\Icons\modern-install-blue.ico"
!define MUI_FINISHPAGE_NOAUTOCLOSE
!define MUI_UNICON "${NSISDIR}\Contrib\Graphics\Icons\modern-uninstall-full.ico"
!define MUI_UNFINISHPAGE_NOAUTOCLOSE
!define MUI_WELCOMEFINISHPAGE_BITMAP "${INSTALLER_DIR}\images\side_b.bmp"
!define MUI_UNWELCOMEFINISHPAGE_BITMAP "${NSISDIR}\Contrib\Graphics\Wizard\orange-uninstall.bmp"
