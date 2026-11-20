# Rho Emacs installer
# (C) 2021 g/christensen
#
# Build from the repository root with `just installer`, or directly:
#   makensis installer\rho.nsi
# The installer executable is written to the repository root.
#
# Layout (installer\):
#   rho.nsi         this file, includes the modules below in dependency order
#   setup.ini       the "Installation options" page
#   src\            settings, libraries, pages, attributes, macros, functions,
#                   page handlers, callbacks, component descriptions
#   sections\       installer sections: core, components, themes, options, post
#   lib\, plugins\  third-party NSIS headers and plugins
#   images\         wizard bitmaps

Unicode True

# makensis starts in the directory of the script. All paths of the payload
# (software\, packages\, elc\) are relative to the repository root, so step
# up to it; INSTALLER_DIR is relative to the root from here on.
!cd ".."
!define INSTALLER_DIR "installer"

!if /FileExists "${INSTALLER_DIR}\rho.nsi"
!else
  !error "The installer script must be compiled from installer\rho.nsi"
!endif

!include "${INSTALLER_DIR}\src\settings.nsh"
!include "${INSTALLER_DIR}\src\libs.nsh"
!include "${INSTALLER_DIR}\src\pages.nsh"
!include "${INSTALLER_DIR}\src\attributes.nsh"
!include "${INSTALLER_DIR}\src\macros.nsh"
!include "${INSTALLER_DIR}\src\functions.nsh"

!include "${INSTALLER_DIR}\sections\core.nsh"
!include "${INSTALLER_DIR}\sections\components.nsh"
!include "${INSTALLER_DIR}\sections\themes.nsh"
!include "${INSTALLER_DIR}\sections\options.nsh"
!include "${INSTALLER_DIR}\sections\post.nsh"

!include "${INSTALLER_DIR}\src\ui.nsh"
!include "${INSTALLER_DIR}\src\callbacks.nsh"
!include "${INSTALLER_DIR}\src\descriptions.nsh"
