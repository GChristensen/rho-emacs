# rho-emacs

[ρEmacs](https://gchristensen.github.io/rho-emacs/) is a preconfigured distribution of the [GNU Emacs](https://www.gnu.org/software/emacs/) editor for
**Microsoft Windows**. It offers some enhancements over the standard **Emacs** experience and targets
several Windows-specific issues.

## Features

- RHO Emacs provides a custom launcher that starts **Emacs** in a
  [server](https://www.gnu.org/software/emacs/manual/html_node/emacs/Emacs-Server.html) mode and then acts like
  [emacsclient](https://www.gnu.org/software/emacs/manual/html_node/emacs/Invoking-emacsclient.html#Invoking-emacsclient).
- The home directory of the standard **Emacs** is bound to `USERPROFILE` or `HOME` environment variables.
  The home directory of **ρEmacs** could be anywhere, and even could be specified as a command-line argument to the launcher.
- A portable installation is possible.
- The [Hack](https://github.com/source-foundry/Hack) typeface, which works quite fast with **Emacs** font-lock mode, is used by default.
  This allows to avoid lags and freezes when editing large files.
- Some common **POSIX** utilities used by **Emacs**, such as **find**/**grep** and **aspell**, are provided by the distribution.
- **ρEmacs** also includes several popular libraries, modes and themes which comprise a reasonably
  convenient configuration out of the box.
- [Capture](https://orgmode.org/manual/Capture.html) with [org-protocol](https://orgmode.org/manual/Protocols.html)
  may be configured by a single check in the installer.
- The preconfigured [org-wiki](https://github.com/caiorss/org-wiki) package allows to create a local **org-mode** based wiki.
- The preconfigured [org-roam](https://www.orgroam.com/) package could be used for non-hierarchical knowledge management.

## Emacs Home Directory

**ρEmacs** installer will ask where do you want to store your personal settings and files. The
following three options are available:

- *Create a dedicated home directory for Emacs* - installer will ask you to manually specify the path of **ρEmacs**
  home directory.
- *Portable installation* - **ρEmacs** will be installed as a portable distribution. In this case **Emacs** will use
  the directory named `home` at the root of the installation as user's personal directory. Installer will also not
  create shortcuts or perform any other system integration.
- *Always use the environment of a current user* - **Emacs** will always use a subfolder named `rho-emacs` inside
  **My Documents** folder of a current user.

## ρEmacs launcher

**ρEmacs** could be launched using a shortcut from **Windows** Start Menu, but the launcher is also accessible
in the command line as the `rho` command. It passes any arguments that do not start with a slash (`/`) to the
underlying program which may be `emacs` if **Emacs** is not running, or `emacsclient` otherwise. The `/HOME`
command-line argument allows to specify the home directory of **ρEmacs** in the following form
`/HOME:path/to/the/directory`, for example: `/HOME:d:/rho-home`. This may be convenient for use in **Windows** shortcuts.

Use `rhoc` command to run **Emacs** in the terminal (it will act as `emacsclient` if **Emacs** is already running).

## ρEmacs enhancement libraries

[ido](https://emacswiki.org/emacs/InteractivelyDoThings),
[fido](https://www.gnu.org/software/emacs/manual/html_node/emacs/Icomplete.html),
[iswitchb](https://www.emacswiki.org/emacs/IswitchBuffers),
[ibuffer](https://www.emacswiki.org/emacs/IbufferMode),
[uniquify](https://www.emacswiki.org/emacs/uniquify),
[save-place](https://www.emacswiki.org/emacs/SavePlace),
[desktop-save](https://www.gnu.org/software/emacs/manual/html_node/elisp/Desktop-Save-Mode.html),
[org-bullets](https://github.com/sabof/org-bullets),
[tabbar](https://www.emacswiki.org/emacs/TabBarMode) and
[bookmark+](https://www.emacswiki.org/emacs/BookmarkPlus) modes could be enabled by default.

**ρEmacs** installer will also offer to activate one of the following UI themes:
[Spacemacs theme](https://github.com/nashamri/spacemacs-theme),
[Moe theme](https://github.com/kuanyui/moe-theme.el),
[Arjen](https://emacsthemes.com/themes/arjen-theme.html). More themes could be selected after the installation.

You may install **ρEmacs** without any enhancements and configure it as you wish.

## Org Mode tools

**ρEmacs** takes care of the configuration of some **org-mode** tools that are very tedious to set up manually:

### org-protocol

The installer is able to automatically configure [org-protocol](https://orgmode.org/manual/Protocols.html)
to capture links and selected text from web browsers. Notably, in the case of western system locales it should
seamlessly work with the **Org Capture** extension
([Chrome](https://chrome.google.com/webstore/detail/org-capture/kkkjlfejijcjgjllecmnejhogpbcigdc?hl=en),
[Firefox](https://addons.mozilla.org/en-US/firefox/addon/org-capture/)) without any additional configuration
(**Emacs** should be running during the capture process). More complex capture schemes
[are possible](https://gchristensen.github.io/posts/dynamic-org-capture-templates/). By default, the captured
content is saved at the file named `~/org/capture.org` (configurable through the
[org-default-notes-file](https://orgmode.org/manual/Setting-up-capture.html#index-org_002ddefault_002dnotes_002dfile)
variable).

### org-wiki

[org-wiki](https://caiorss.github.io/org-wiki/) package offers a firm foundation for a personal knowledge base.
Its [clip.jar](https://github.com/caiorss/org-wiki#paste-image) utility is included into the distribution
(a private Java runtime necessary to run it is provided by the installer). The default wiki root directory is
located at `~/org/wiki`.

**org-wiki** will install the [helm](https://github.com/emacs-helm/helm) package with its dependencies on the
first run of **Emacs**, so Internet connection is required.

### org-roam

**org-roam** package could be used for non-hierarchical knowledge management, such as Zettelkasten. Its
**emacsql-sqlite.exe** utility is already precompiled, so no compiler installation is necessary. **org-roam**
files and database are stored in the `~/org-roam` directory. The following key-bindings are available:

| Key | Command |
| --- | --- |
| `C-c n f` | org-roam-node-find |
| `C-c n r` | org-roam-node-random |
| `C-c n i` | org-roam-node-insert |
| `C-c n o` | org-id-get-create |
| `C-c n t` | org-roam-tag-add |
| `C-c n a` | org-roam-alias-add |
| `C-c n l` | org-roam-buffer-toggle |

## Installing Third Party Emacs Extensions

### Automated installation

Automated package installation is possible through the [MELPA](https://melpa.org/) repository available with
the `M-x package-list-packages` command.

### Manual installation

To install a package manually, you need to place it into a subdirectory of `~/.emacs.d` folder (where `~` is a
shortcut for the home directory). Then you need to add the following line to your `~/.emacs` configuration file:

```elisp
(add-to-list 'load-path "~/.emacs.d/my-library/")
```

to add the package to **Emacs** load list. Then you should initialize the package according to its manual, for
example, by placing `(require 'my-library)` line into `~/.emacs`.

## Changelog

#### 2026-09-5 v1.2.6

* Updated to Emacs 31.1.

[Full changelog](changelog.md)
