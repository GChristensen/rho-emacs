;;; -- Inst_Emacs.iss --

;; Part of EmacsW32

;; Author:     Lennart Borgman <lennart.borgman.073@student.lu.se>
;; Version:    1.58
;; Keywords:   emacs, install

;; Copyright (C) 2005, 2006 Lennart Borgman

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public
;; License along with Emacs; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA


;;; Commentary:

;; This file is used by Inno Setup to create the EmacsW32 for MS
;; Windows package. It has been tested with Inno Setup version 5.1.5
;; (released 2005-08-18).

;; Note: I got some access violation in the custom dialog pages for
;; file assoc when the pages where displayed a second time. After
;; setting array len to 0 before setting them to their actual valus
;; the acc viol seems to have disappeared.

;;; History:




[CustomMessages]
Need22AskCont=Need Emacs version 22 or higher. This file name suggest that it is a lower version. Continue?
ErrCantContinue=Can not continue. Exiting.
RegFileNotFound=Can not find file%n    {val1}%n%nThis path is in the registry at%n    {val2}%n%nThis is just an informational message.%nPlease continue with the installation.
CantWriteGsTempEl=Can not write gs lisp setup file "{value}"
MustEndInTarGz=Sorry, files to unpack must be gzipped tar files, ending in .tar.gz
FailedExtractTmp=Failed extracting "{val1}" to temp file. Error: {val2}
ExtrTmpNotFound=Temporary file "{value}" not found
CantFindFreeTempName=Can not find free temporary file name with base "{value}"
TempTarAlreadyExists=Temporary tar file "{value}" already exists!?
UnpackingTarGzFailed=Unpacking of the .tar.gz file failed
TempTarNotFound=Temporary tar file "{value}" not found
UnpackingTarFailed=Unpacking of the temporary .tar file failed
TarOutputNotFound=Expected output dir from tar file not found: "{value}"
RenameFailed=Could not rename "{value}"
RemoveTempTarDirFailed=Could not remove temp dir "{value}"
CopySiteLisp2Failed=Could not copy inner site-lisp
RenameSiteLisp2Failed=Could not rename inner site-lisp
UnpackingTarGzFailed=Unpacking of .tar.gz file failed
DefaultElNoEmacsExe=Can not setup site-lisp/default.el because emacs.exe was not found
   ;CustomNeedsBase=Basic Emacs setup must be done before any other customization. (The installer does not know if the basic Emacs setup is already done.)
CantAddHomeEnv=Can not add user HOME to environment variables
CantCreateHomeDir=Can not create user HOME directory "{value}"
BadSubDirName=Subdirectory name can not contain%nany of the following characters:%n%n{value}
UnpackEmacsNoBasic=If you unpack Emacs without choosing also "Basic Emacs setup" you will miss integration with MS Windows. Do you want to add "Basic Emacs setup"?
ForAllUsers=Do you want to install for all users? ('Yes' will install for all users while 'No' will install only for the current user.)
ForAllUsersDelHKCU=Delete same values for this users (so that the values for all users take effect)?
WarnDelEmacsHKCU=Will delete this registry string value:%n%nHKCU\Software\GNU\Emacs\emacs_dir
WarnFailedDelEmacsHKCU=Failed to delete HKCU\Software\GNU\Emacs

InternalError=Internal Error
PleaseReport=Please find out if this bug has been reported and report it otherwise!

NoEmacsFound=Since you did not select to unpack an Emacs binary distribution file you must have Emacs installed to proceed. However Emacs path was not found in the registry.%n%n** Note 1: If you have installed Emacs but did not run AddPm.exe please do this. Then restart the installation.%n%n** Note 2: The environmental variable Path was also searched to find emacs.exe.

FileAssocChgHdr=From the list of extensions you gave file types and related associations have been investigated.

CapHomePg=Home Directory
Sub1HomePg=Environment variable HOME undefined
Sub2HomePg=Emacs and most other unix-derived applications store their configuration files in the directory pointed to by the environment variable HOME. Setup can set this environment variable for you if you want.
ChkDontSetHome=Do not set environment variable HOME.

CapSetHomePg=Select Home Directory
Sub1SetHomePg=Please select an appropriate directory
;Sub2SetHomePg=Neither the directory name nor the path to the directory should contain spaces.
Sub2SetHomePg=Give the name of the directory to use as your HOME directory. Default is the directory which is normally called "My Documents". The value you enter will be stored in the registry so that the environmental variable HOME will be set when you logon.





[Messages]

;SetupWindowTitle=Setup - %1
SetupWindowTitle=Setup - EmacsW32 Install Wizard

;NoUninstallWarningTitle=Components Exist
NoUninstallWarningTitle=Tasks Already Done

;NoUninstallWarning=Setup has detected that the following components are already installed on your computer:%n%n%1%n%nDeselecting these components will not uninstall them.%n%nWould you like to continue anyway?
NoUninstallWarning=This is just an information message. Deselecting components will not uninstall them. Setup has detected that the following tasks may already have been done on your computer but where deselected now:%n%n%1%n%n%nWould you like to continue to next step?

;ReadyMemoComponents=Selected components:
ReadyMemoComponents=Selected components and tasks:

;DiskSpaceMBLabel=At least [mb] MB of free disk space is required.
DiskSpaceMBLabel=If you have already installed Emacs at least [mb] MB of additional free disk space is required (2*70 MB more if you unpack a full distribution file).

; *** "Select Components" wizard page
;WizardSelectComponents=Select Components

;SelectComponentsDesc=Which components should be installed?
SelectComponentsDesc=What should be done?

;SelectComponentsLabel2=Select the components you want to install; clear the components you do not want to install. Click Next when you are ready to continue.
SelectComponentsLabel2=Select the tasks you want the setup program to perform; clear the tasks you do not want to be done. Click Next when you are ready to continue.

;WelcomeLabel1=Welcome to the [name] Install Wizard
;WelcomeLabel1=Welcome to [name] for MS Windows
WelcomeLabel1=Welcome to the [name] Install Wizard

;WelcomeLabel2=This will install [name/ver] on your computer.%n%nIt is recommended that you close all other applications before continuing.
WelcomeLabel2=This will install [name/ver] which contains some adjustments for Emacs hopefully helpful on MS Windows.%n%nIMPORTANT: [name] does NOT include Emacs itself. If you do not have Emacs already you must download the binary distribution files for Emacs on W32 separately. Setup can tell you where to find them and help you unpack them.%n%nFor more information click Help below to the left. If Emacs is running please close it before you continue.

SelectDirDesc=Where should [name] be installed?
;SelectDirDesc=Where should Emacs be installed?

SelectDirLabel3=Setup will install [name] into the following folder.
;SelectDirLabel3=Setup will install or look for Emacs into the following folder.

;SelectTasksLabel2=Select the additional tasks you would like Setup to perform while installing [name], then click Next.
SelectTasksLabel2=Select the additional tasks you would like Setup to perform while installing Emacs, then click Next.

;ReadyLabel1=Setup is now ready to begin installing [name] on your computer.
ReadyLabel1=[name] is now ready to do the tasks below on your computer.

;PreparingDesc=Setup is preparing to install [name] on your computer.
PreparingDesc=[name] is preparing tasks to do on your computer.

;PreviousInstallNotCompleted=The installation/removal of a previous program was not completed. You will need to restart your computer to complete that installation.%n%nAfter restarting your computer, run Setup again to complete the installation of [name].
PreviousInstallNotCompleted=The installation/removal of a previous program was not completed. You will need to restart your computer to complete that installation.%n%nAfter restarting your computer, run Setup again to complete the installation of Emacs.

;InstallingLabel=Please wait while Setup installs [name] on your computer.
InstallingLabel=Please wait while Setup installs Emacs on your computer.

;FinishedHeadingLabel=Completing the [name] Setup Wizard
FinishedHeadingLabel=Completing [name]

;FinishedLabelNoIcons=Setup has finished installing [name] on your computer.
FinishedLabelNoIcons=Setup has finished installing Emacs on your computer.

;FinishedLabel=Setup has finished installing [name] on your computer. The application may be launched by selecting the installed icons.
FinishedLabel=Setup has finished installing Emacs on your computer. The application may be launched by selecting the installed icons. %n%nA command file named emacs.cmd has been written in the Emacs directory. Copy this to your path to use Emacs from the command line.

;FinishedRestartLabel=To complete the installation of [name], Setup must restart your computer. Would you like to restart now?
;FinishedRestartMessage=To complete the installation of [name], Setup must restart your computer.%n%nWould you like to restart now?



StatusRollback=Rolling back changes that are possible to roll back...
BeveledLabel=EmacsW32

;SetupLdrStartupMessage=This will install %1. Do you wish to continue?

;ExitSetupMessage=Setup is not complete. If you quit now, the program will not be installed.%n%nYou may run the Setup program again at another time to complete the installation.%n%nExit Setup?


;; {****************************************************************************}
;; {****************************************************************************}
;; {****************************************************************************}
;; {****************************************************************************}

[Setup]
VersionInfoVersion=1.58
AppName=EmacsW32
AppVerName=EmacsW32 (ver 1.58)
OutputBaseFilename=EmacsW32-alone-1.58

AppComments=This is free software
;;AppContact=http://www.EmacsWiki.org/
;;AppPublisher=http://www.EmacsWiki.org/
;;AppPublisherUrl=http://www.EmacsWiki.org/
;;AppReadmeFile=ReadMe.txt
AppSupportUrl=http://www.EmacsWiki.org/
AppUpdatesUrl=http://www.EmacsWiki.org/

AllowCancelDuringInstall=no
ChangesAssociations=yes
ChangesEnvironment=yes
Compression=lzma/normal
DefaultDirName={pf}\Emacs
DefaultGroupName=GNU Emacs
DisableDirPage=yes
;DisableFinishedPage=yes
DisableProgramGroupPage=yes
;DisableReadyMemo=yes
;DisableReadyPage=yes
;DisableStartupPrompt=yes
InfoBeforeFile=InfoBeforeEmacsW32.txt
LicenseFile=License.txt
PrivilegesRequired=none
SolidCompression=yes
Uninstallable=no
UsePreviousAppDir=no
UsePreviousTasks=no
;;WizardImageFile=etc\img\WizImg1.bmp
WizardImageFile=compiler:WizModernImage-IS.bmp
WizardSmallImageFile=etc\img\WizImgSmall1.bmp

[UninstallDelete]
Type: files;      Name: "{code:AppSubDir|EmacsW32\ediff.cmd}"
Type: files;      Name: "{code:AppSubDir|EmacsW32\emacs}"
Type: dirifempty; Name: "{code:AppSubDir|EmacsW32}"
   ;;Type: files;      Name: "{code:AppSubDir|EmacsW32\tmp\EmacsW32.iss}"
   ;;Type: files;      Name: "{code:AppSubDir|EmacsW32\tmp\OldSetupTemp.el}"
   ;;Type: files;      Name: "{code:AppSubDir|EmacsW32\tmp\SetupTemp.el}"
   ;;Type: dirifempty; Name: "{code:AppSubDir|EmacsW32\tmp}"
   ;;Type: dirifempty; Name: "{code:AppSubDir|site-lisp}"
   ;;Type: dirifempty; Name: "{code:AppSubDir|my-lisp}"
   ;;Type: dirifempty; Name: "{code:AppSubDir|}"


[Files]
;; First files that might be temporary extracted:
Source: "etc\HelpEmacsW32InstWiz.html";        DestDir: "{code:AppSubDir|EmacsW32\etc}";  Components: base;
Source: "etc\HelpEmacsEmacsW32InstWiz.html";   DestDir: "{code:AppSubDir|EmacsW32\etc}";  Components: base;
Source: "etc\iw-select-tasks-ew32.png";        DestDir: "{code:AppSubDir|EmacsW32\etc}";  Components: base;
Source: "etc\iw-select-tasks-emacs+ew32.png";  DestDir: "{code:AppSubDir|EmacsW32\etc}";  Components: base;
Source: "etc\iw-file-type-associations.png";   DestDir: "{code:AppSubDir|EmacsW32\etc}";  Components: base;
Source: "etc\iw-change-file-associations.png"; DestDir: "{code:AppSubDir|EmacsW32\etc}";  Components: base;
Source: "etc\iw-ready-to-install.png";         DestDir: "{code:AppSubDir|EmacsW32\etc}";  Components: base;
Source: "etc\bar.js";                      DestDir: "{code:AppSubDir|EmacsW32\etc}";  Components: base;
Source: "etc\EmacsW32.css";                DestDir: "{code:AppSubDir|EmacsW32\etc}";  Components: base;
Source: "etc\EmacsW32Util.html";           DestDir: "{code:AppSubDir|EmacsW32\etc}";  Components: base;
Source: "etc\EmacsW32.js";                 DestDir: "{code:AppSubDir|EmacsW32\etc}";  Components: base;
Source: "etc\EmacsW32SetupUtilities.html"; DestDir: "{code:AppSubDir|EmacsW32\etc}";  Components: base;
Source: "etc\html.js";                     DestDir: "{code:AppSubDir|EmacsW32\etc}";  Components: base;
Source: "etc\td_oc.css";                   DestDir: "{code:AppSubDir|EmacsW32\etc}";  Components: base;
Source: "etc\td_oc.js";                    DestDir: "{code:AppSubDir|EmacsW32\etc}";  Components: base;
Source: "etc\w32-build-emacs.html";        DestDir: "{code:AppSubDir|EmacsW32\etc}";  Components: base;
Source: "etc\w32-unix-progs.html";         DestDir: "{code:AppSubDir|EmacsW32\etc}";  Components: base;
Source: "etc\img\WizImgSmall1.bmp";        DestDir: "{code:AppSubDir|EmacsW32\etc\img}"; Components: base;
Source: "etc\img\emacsw32-gnu.png";        DestDir: "{code:AppSubDir|EmacsW32\etc\img}"; Components: base;
Source: "etc\img\P1010509-sheep-128.JPG";  DestDir: "{code:AppSubDir|EmacsW32\etc\img}"; Components: base;
Source: "etc\img\P1000488-bear-128.JPG";   DestDir: "{code:AppSubDir|EmacsW32\etc\img}"; Components: base;
Source: "bin\7za.exe";                     DestDir: "{code:AppSubDir|EmacsW32\bin}"; Components: base;

;; <<Emacs itself here>>

Source: "gnuwin32\*";       DestDir: "{code:AppSubDir|EmacsW32\gnuwin32}"; Flags: recursesubdirs replacesameversion; Components: base;
Source: "nxhtml\*";         DestDir: "{code:AppSubDir|EmacsW32\nxhtml}"; Flags: recursesubdirs replacesameversion; Components: base;


Source: "NotReadme.txt";    DestDir: "{app}"; Components: base;

Source: "EmacsW32.iss";             DestDir: "{code:AppSubDir|EmacsW32}"; Components: base;
Source: "NotReadme.txt";            DestDir: "{code:AppSubDir|EmacsW32}"; Components: base;
Source: "ReadMe.txt";               DestDir: "{code:AppSubDir|EmacsW32}"; Components: base;
Source: "License.txt";              DestDir: "{code:AppSubDir|EmacsW32}"; Components: base;
Source: "InfoBeforeEmacsW32.txt";   DestDir: "{code:AppSubDir|EmacsW32}"; Components: base;

Source: "bin\emacs-w32-build-check.pl"; DestDir: "{code:AppSubDir|EmacsW32\bin}"; Components: base;
;;Source: "bin\gnuclient.exe";            Destdir: "{code:AppSubDir|EmacsW32\bin}"; Components: base;
;;Source: "bin\gnuclientw.exe";           Destdir: "{code:AppSubDir|EmacsW32\bin}"; Components: base;
;;Source: "bin\gnudoit.exe";              Destdir: "{code:AppSubDir|EmacsW32\bin}"; Components: base;
;;Source: "bin\gnuserv.exe";              Destdir: "{code:AppSubDir|EmacsW32\bin}"; Components: base;
;;Source: "bin\gnuservauto.el";           DestDir: "{code:AppSubDir|EmacsW32\bin}"; Components: base;
;;Source: "bin\gnuserv.el";               DestDir: "{code:AppSubDir|EmacsW32\bin}"; Components: base;
Source: "bin\usethis.exe";              Destdir: "{code:AppSubDir|EmacsW32\bin}"; Components: base;
Source: "bin\w32-reg-iface.exe";        Destdir: "{code:AppSubDir|EmacsW32\bin}"; Components: base;
Source: "bin\winforms.exe";             Destdir: "{code:AppSubDir|EmacsW32\bin}"; Components: base;

Source: "bin\usethis.exe";          Destdir: "{code:emacs_dir_ForCode|bin}"; Components: base;

Source: "bin\src\readme.txt";        Destdir: "{code:AppSubDir|EmacsW32\bin\src}"; Components: base;
;;Source: "bin\src\gnuserv.zip";       Destdir: "{code:AppSubDir|EmacsW32\bin\src}"; Components: base;
;;Source: "bin\src\w32-reg-iface.zip"; Destdir: "{code:AppSubDir|EmacsW32\bin\src}"; Components:base;
;;Source: "bin\src\winforms.zip";      Destdir: "{code:AppSubDir|EmacsW32\bin\src}"; Components: base;



Source: "lisp\cmd-mode.el";           DestDir: "{code:AppSubDir|EmacsW32\lisp}"; Components: base;
Source: "lisp\dos.el";                DestDir: "{code:AppSubDir|EmacsW32\lisp}"; Components: base;
Source: "lisp\emacsw32.el";           DestDir: "{code:AppSubDir|EmacsW32\lisp}"; Components: base;
Source: "lisp\emacsw32-custom.el";    DestDir: "{code:AppSubDir|EmacsW32\lisp}"; Components: base;
Source: "lisp\emacsw32-eol.el";       DestDir: "{code:AppSubDir|EmacsW32\lisp}"; Components: base;
Source: "lisp\menuacc.el";            DestDir: "{code:AppSubDir|EmacsW32\lisp}"; Components: base;
Source: "lisp\noprint.el";            DestDir: "{code:AppSubDir|EmacsW32\lisp}"; Components: base;
Source: "lisp\powershell.el";         DestDir: "{code:AppSubDir|EmacsW32\lisp}"; Components: base;
Source: "lisp\powershell-mode.el";    DestDir: "{code:AppSubDir|EmacsW32\lisp}"; Components: base;
Source: "lisp\w32-print.el";          DestDir: "{code:AppSubDir|EmacsW32\lisp}"; Components: base;
Source: "lisp\nxhtml-loader.el";      DestDir: "{code:AppSubDir|EmacsW32\lisp}"; Components: base;
Source: "lisp\usb-setup.el";          DestDir: "{code:AppSubDir|EmacsW32\lisp}"; Components: base;
;;Source: "lisp\visual-basic-mode.el";  DestDir: "{code:AppSubDir|EmacsW32\lisp}"; Components: base;
Source: "lisp\w32-grep.el";           DestDir: "{code:AppSubDir|EmacsW32\lisp}"; Components: base;
Source: "lisp\w32-integ.el";          DestDir: "{code:AppSubDir|EmacsW32\lisp}"; Components: base;
Source: "lisp\w32-meta.el";           DestDir: "{code:AppSubDir|EmacsW32\lisp}"; Components: base;
Source: "lisp\w32-reg-iface.el";      DestDir: "{code:AppSubDir|EmacsW32\lisp}"; Components: base;
Source: "lisp\w32-regdat.el";         DestDir: "{code:AppSubDir|EmacsW32\lisp}"; Components: base;
Source: "lisp\w32shell.el";           DestDir: "{code:AppSubDir|EmacsW32\lisp}"; Components: base;

Source:"lisp\mkInstaller.el";           DestDir: "{code:AppSubDir|EmacsW32\lisp}"; Components: base;
Source:"lisp\mkZipLst.el";              DestDir: "{code:AppSubDir|EmacsW32\lisp}"; Components: base;
Source:"lisp\emacsw32-setup-base.el";   DestDir: "{code:AppSubDir|EmacsW32\lisp}"; Components: base;
Source:"lisp\emacsw32-setup-custom.el"; DestDir: "{code:AppSubDir|EmacsW32\lisp}"; Components: base;
Source:"lisp\setup-helper.el";          DestDir: "{code:AppSubDir|EmacsW32\lisp}"; Components: base;


[Dirs]
Name: "{code:AppSubDir|site-lisp}"; Components: base;
;Name: "{code:AppSubDir|my-lisp}";   Components: base;
Name: "{code:AppSubDir|EmacsW32}";   Components: base;
Name: "{code:AppSubDir|EmacsW32\tmp}";   Components: base;


;;Name: "{group}\Emacs Gnuclient"; Filename: "{code:AppSubDir2|EmacsW32\bin\gnuclientw.exe}"; Parameters: "-sqf"; WorkingDir: "%home%"; Components: win\winshcut;
;;Name: "{userdesktop}\Emacs Gnuclient"; Filename: "{code:AppSubDir2|EmacsW32\bin\gnuclientw.exe}"; Parameters: "-sqf"; WorkingDir: "%home%"; Components: win\winshcut; Check: ForCurrentUser;
;;Name: "{commondesktop}\Emacs Gnuclient"; Filename: "{code:AppSubDir2|EmacsW32\bin\gnuclientw.exe}"; Parameters: "-sqf"; WorkingDir: "%home%"; Components: win\winshcut; Check: ForAllUsers;
;;Name: "{userappdata}\Microsoft\Internet Explorer\Quick Launch\Emacs Gnuclient"; Filename: "{code:AppSubDir2|EmacsW32\bin\gnuclientw.exe}"; Parameters: "-sqf"; WorkingDir: "%home%"; Components: win\winshcut;
;;Name: "{commonappdata}\Microsoft\Internet Explorer\Quick Launch\Emacs Gnuclient"; Filename: "{code:AppSubDir2|EmacsW32\bin\gnuclientw.exe}"; Parameters: "-sqf"; WorkingDir: "%home%"; Components: win\winshcut; Check: ForAllUsers;
;;Name: "{sendto}\Emacs Gnuclient"; Filename: "{code:AppSubDir2|EmacsW32\bin\gnuclientw.exe}"; Parameters: "-sqf"; Components: win\winshcut;

[Icons]
;; Pause
;fix-me: C:\emacs\p\100203\emacs\bin\runemacs.exe -Q -l "C:\emacs\p\100203\emacsw32\nxhtml\util\pause.el" --geometry=40x3 -D --eval "(pause-start 20 nil)"
Name: "{commonstartup}\Emacs Pause"; Filename: "{code:Emacs_dir_ForCode|bin\runemacs.exe}"; Parameters: "-Q -l ""{code:AppSubDir|EmacsW32\nxhtml\util\pause.el}"" --geometry=40x3 -D --eval ""(pause-start 20 nil)"""; Check: ForAllUsers; Components: pause;
Name: "{userstartup}\Emacs Pause"; Filename: "{code:Emacs_dir_ForCode|bin\runemacs.exe}"; Parameters: "-Q -l ""{code:AppSubDir|EmacsW32\nxhtml\util\pause.el}"" --geometry=40x3 -D --eval ""(pause-start 20 nil)"""; Check: ForCurrentUser; Components: pause;
Name: "{group}\EmacsClient"; Filename: "{code:emacs_dir_ForCode|bin\emacsclientw.exe}"; Parameters: "-n"; WorkingDir: "%home%"; Components: win\winshcut;
Name: "{userdesktop}\GNU EmacsClient"; Filename: "{code:emacs_dir_ForCode|bin\emacsclientw.exe}"; Parameters: "-n"; WorkingDir: "%home%"; Components: win\winshcut; Check: ForCurrentUser;
Name: "{commondesktop}\GNU EmacsClient"; Filename: "{code:emacs_dir_ForCode|bin\emacsclientw.exe}"; Parameters: "-n"; WorkingDir: "%home%"; Components: win\winshcut; Check: ForAllUsers;
;; Note: There is no common Quick Launch bar
Name: "{userappdata}\Microsoft\Internet Explorer\Quick Launch\GNU EmacsClient"; Filename: "{code:emacs_dir_ForCode|bin\emacsclientw.exe}"; Parameters: "-n"; WorkingDir: "%home%"; Components: win\winshcut;
;; Note: There is no common Send To folder
Name: "{sendto}\GNU EmacsClient"; Filename: "{code:emacs_dir_ForCode|bin\emacsclientw.exe}"; Parameters: "-n"; Components: win\winshcut;

[Types]
;;Name: "full"; Description: "Full installation of Emacs+EmacsW32"; Check: CanWriteHKLM;
;;Name: "full"; Description: "Full installation of Emacs+EmacsW32 (without priveleges)"; Check: IsUser;
Name: "full"; Description: "Full installation of Emacs+EmacsW32";
Name: "minimal"; Description: "Minimal installation of Emacs+EmacsW32"
;;Name: "fullw32"; Description: "Full installation of EmacsW32"; Check: InstalledAndCanWriteHKLM;
;;Name: "fullw32"; Description: "Full installation of EmacsW32 (without priveleges)"; Check: InstalledAndIsUser;
Name: "fullw32"; Description: "Full installation of EmacsW32";
Name: "minw32"; Description: "Minimal installation of EmacsW32"
Name: "onlyunpack"; Description: "Just unpack Emacs distribution file";
Name: "custom"; Description: "Custom installation"; Flags: iscustom


[Components]
Types: full minimal onlyunpack; Name: "unpack"; Description: "Unpack Emacs binary distribution file"; ExtraDiskSpaceRequired:146000000;
Types: full fullw32; Name: "win"; Description: "Windows Integration of EmacsClient";
Types: full fullw32; Name: "win\winshcut"; Description: "EmacsClient shortcuts (Desktop, SendTo, Start Programs, Quick Launch)";
;;Types: full fullw32; Name: "win\assoc"; Description: "Associate selected file types with EmacsClient"; Check: CanWriteHKLM;
Types: full fullw32; Name: "win\assoc"; Description: "Associate selected file types with EmacsClient";
Types: full fullw32 minimal minw32; Name: "base"; Description: "Install EmacsW32 Files (includes quick printing)";
Types: full fullw32 minimal minw32; Name: "pause"; Description: "Add Emacs Pause to Startup";

;;Types: full fullw32; Name: "win\home"; Description: "Set User HOME environment variable"; Check: HasNoHome;
;;Types: full fullw32; Name: "win\web-edit"; Description: "Use Emacs as source viewer for web browsers"; Check: CanWriteHKLM;
;;Types: full fullw32; Name: "win\web-edit\ie"; Description: "Use Emacs as source viewer in Internet Explorer"; Check: CanWriteHKLM;


[Tasks]
;Name: "wiki"; Description: "Emacs Wiki - creating Wiki webs on your pc";
;Name: "vi"; Description: "Setup VI key bindings (viper)"; Flags: unchecked;


[Run]
;;;; Entries here are shown on the finish page!
Filename: "{code:emacs_dir_ForCode|bin\emacsclientw.exe}"; Parameters: "-n -e ""(emacsw32-show-custstart)"""; Flags: postinstall nowait runasoriginaluser; Description: "Start Emacs with Emacsw32 Options Page"; Check: BasicAndNotSilent;

[UninstallRun]
; there is perhaps a BUG - this will run at installation! (Documented, changed in ver 5)
;Filename: "notepad.exe"; Check: TellNoEmacsDel;

[UninstallDelete]
Type: files; Name: "{app}\EmacsW32\elc.cmd";
Type: files; Name: "{app}\EmacsW32\emacs.cmd";
Type: files; Name: "{app}\EmacsW32\emacs.sh";
Type: files; Name: "{app}\EmacsW32\mkBase.cmd";
Type: files; Name: "{app}\EmacsW32\mkInstaller.cmd";
Type: files; Name: "{app}\EmacsW32\mkSetupHelper.cmd";
Type: files; Name: "{app}\EmacsW32\mkZip.cmd";

[InstallDelete]
;;Type: filesandordirs; Name: "{app}\emacs";

[Registry]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AddPm actions (uninstallable, from Frank Schmitt)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Must be doubled, can not use scripted parameters for HKLM
;; Brian Elmegaard detect bad behaviour here:
Root: HKLM; Subkey: "Software\GNU"; Flags: uninsdeletekeyifempty; Check: ForAllUsers; Components: base
Root: HKLM; Subkey: "Software\GNU\Emacs"; Flags: uninsdeletekey; Check: ForAllUsers; Components: base
Root: HKLM; Subkey: "Software\GNU\Emacs"; ValueType: expandsz; ValueName: "emacs_dir"; ValueData: "{code:emacs_dir_ForCode|}"; Flags: uninsdeletekey; Check: ForAllUsers; Components: base

;;Root: HKCU; Subkey: "Software\GNU"; Flags: uninsdeletekeyifempty; Check: BaseAndIsUser; Components: base
;;Root: HKCU; Subkey: "Software\GNU\Emacs"; Flags: uninsdeletekey; Check: BaseAndIsUser; Components: base
;;Root: HKCU; Subkey: "Software\GNU\Emacs"; ValueType: expandsz; ValueName: "emacs_dir"; ValueData: "{code:emacs_dir_ForCode|}"; Flags: uninsdeletekey; Check: BaseAndIsUser; Components: base
Root: HKCU; Subkey: "Software\GNU"; Flags: uninsdeletekeyifempty; Check: ForCurrentUser; Components: base
Root: HKCU; Subkey: "Software\GNU\Emacs"; Flags: uninsdeletekey; Check: ForCurrentUser; Components: base
Root: HKCU; Subkey: "Software\GNU\Emacs"; ValueType: expandsz; ValueName: "emacs_dir"; ValueData: "{code:emacs_dir_ForCode|}"; Flags: uninsdeletekey; Check: ForCurrentUser; Components: base

;; By registering your application under the App Paths key, users can start your application from Explorer's Run dialog by entering just the EXE's filename and no path.
;;Root: HKLM; Subkey: "SOFTWARE\Microsoft\Windows\CurrentVersion\App Paths\EmacsW32"; ValueType: string; ValueName: ""; ValueData: "{app}\emacsclientw.exe"; Flags: uninsdeletekey


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Context menu (use SendTo instead!)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Root: HKLM; Subkey: "*\shell\emacs"; ValueType: string; ValueName: ""; ValueData: "Open with Emacs ..." Check: CanWriteHKLM;
;Root:HKLM; Subkey: "*\shell\emacs\command"; ValueType: string; ValueName: ""; ValueData: "{code:OpenAction|}"; Check: CanWriteHKLM;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; IE View Source
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Does not work...
;;Components: "win\web-edit\ie"; Root: HKLM; Subkey: "htmlfile\shell\edit\command"; ValueType: string; ValueName: ""; ValueData: "{code:OpenAction|}"; Check: CanWriteHKLM;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; File Associations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; http://www.brianmadden.com/content/content.asp?ID=168
;;
;; File type information and associations are stored in the
;; HKEY_CLASSES_ROOT (HKCR) hive of the registry. HKCR is built-up
;; from two locations: HKEY_LOCAL_MACHINE\SOFTWARE\Classes and
;; HKEY_CURRENT_USER\SOFTWARE\Classes. If a subkey or entry appears in
;; either location, it automatically appears in HKCR. If there are any
;; conflicts in the source keys, the HKCU takes precedence. (This is a
;; good thing and what fundamentally allows us to do per-user file
;; associations.)

;;;;;;;;;;;;;;;;;; File extension part (similar to command line "assoc")
;;Root: HKCR; Subkey: ".el"; ValueType: string; ValueName: ""; ValueData: "EmacsFile"; Check: CanWriteHKLM; Components: win\assoc;
Root: HKLM; Subkey: "SOFTWARE\Classes\.el"; ValueType: string; ValueName: ""; ValueData: "EmacsFile"; Check: ForAllUsers; Components: win\assoc;
Root: HKCU; Subkey: "SOFTWARE\Classes\.el"; ValueType: string; ValueName: ""; ValueData: "EmacsFile"; Check: ForCurrentUser; Components: win\assoc;


;;;;;;;;;;;;;;;;;; Common for file associations (similar to command line "ftype")

;;; FIX-ME: Remove HKCU when needed below!

;; File type name:
;;Root: HKCR; Subkey: "EmacsFile"; ValueType: string; ValueName: ""; ValueData: "Emacs File"; Check: CanWriteHKLM; Components: win\assoc;
Root: HKLM; Subkey: "SOFTWARE\Classes\EmacsFile"; ValueType: string; ValueName: ""; ValueData: "Emacs File"; Check: ForAllUsers; Components: win\assoc;
Root: HKCU; Subkey: "SOFTWARE\Classes\EmacsFile"; ValueType: string; ValueName: ""; ValueData: "Emacs File"; Check: ForCurrentUser; Components: win\assoc;

;; Default icon:
;;Root: HKCR; Subkey: "EmacsFile\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "{code:AppSubDir2|Emacs\bin\emacsclientw.EXE,0}"; Check: CanWriteHKLM; Components: win\assoc;
;;Root: HKLM; Subkey: "SOFTWARE\Classes\EmacsFile\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "{code:AppSubDir2|Emacs\bin\emacsclientw.EXE,0}"; Check: ForAllUsers; Components: win\assoc;
Root: HKLM; Subkey: "SOFTWARE\Classes\EmacsFile\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "{code:emacs_dir_ForCode|bin\emacsclientw.EXE,0}"; Check: ForAllUsers; Components: win\assoc;
;;Root: HKCU; Subkey: "SOFTWARE\Classes\EmacsFile\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "{code:AppSubDir2|Emacs\bin\emacsclientw.EXE,0}"; Check: ForCurrentUser; Components: win\assoc;
Root: HKCU; Subkey: "SOFTWARE\Classes\EmacsFile\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "{code:emacs_dir_ForCode|bin\emacsclientw.EXE,0}"; Check: ForCurrentUser; Components: win\assoc;

;; Double clicking:
;;Root: HKCR; Subkey: "EmacsFile\shell\open\command"; ValueType: string; ValueName: ""; ValueData: "{code:OpenAction|}"; Check: CanWriteHKLM; Components: win\assoc;
Root: HKLM; Subkey: "SOFTWARE\Classes\EmacsFile\shell\open\command"; ValueType: string; ValueName: ""; ValueData: "{code:OpenAction|}"; Check: ForAllUsers; Components: win\assoc;
Root: HKCU; Subkey: "SOFTWARE\Classes\EmacsFile\shell\open\command"; ValueType: string; ValueName: ""; ValueData: "{code:OpenAction|}"; Check: ForCurrentUser; Components: win\assoc;

;; Supplemental verb
Root: HKLM; Subkey: "SOFTWARE\Classes\SystemFileAssociations\text\shell\edit.Emacs"; ValueType: string; ValueName: ""; ValueData: "Edit with Emacs"; Check: CanWriteHKLM; Components: win\assoc;
Root: HKLM; Subkey: "SOFTWARE\Classes\SystemFileAssociations\text\shell\edit.Emacs\command"; ValueType: string; ValueName: ""; ValueData: "{code:OpenAction|}"; Check: CanWriteHKLM; Components: win\assoc;
Root: HKCU; Subkey: "SOFTWARE\Classes\SystemFileAssociations\text\shell\edit.Emacs"; ValueType: string; ValueName: ""; ValueData: "Edit with Emacs"; Check: ForCurrentUser; Components: win\assoc;
Root: HKCU; Subkey: "SOFTWARE\Classes\SystemFileAssociations\text\shell\edit.Emacs\command"; ValueType: string; ValueName: ""; ValueData: "{code:OpenAction|}"; Check: ForCurrentUser; Components: win\assoc;

;; Open with list
Root: HKLM; Subkey: "SOFTWARE\Classes\Applications\EmacsClientW.exe\shell\edit\command"; ValueType: string; ValueName: ""; ValueData: "{code:OpenAction|}"; Check: CanWriteHKLM; Components: win\assoc;
Root: HKCU; Subkey: "SOFTWARE\Classes\Applications\EmacsClientW.exe\shell\edit\command"; ValueType: string; ValueName: ""; ValueData: "{code:OpenAction|}"; Check: ForCurrentUser; Components: win\assoc;

Root: HKLM; Subkey: "SOFTWARE\Classes\SystemFileAssociations\text\OpenWithList\EmacsClientW.exe"; ValueType: string; ValueName: ""; ValueData: ""; Check: CanWriteHKLM; Components: win\assoc;
Root: HKCU; Subkey: "SOFTWARE\Classes\SystemFileAssociations\text\OpenWithList\EmacsClientW.exe"; ValueType: string; ValueName: ""; ValueData: ""; Check: ForCurrentUser; Components: win\assoc;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END - File Associations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;







;; {;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;}
;;;; Code begin
;; {;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;}
[Code]

{ Classes does not seem to be implemented here,
  or I just do not understand the syntax???
TAssForm = class(TForm)
end;
}

const
   g_bIncludesEmacs     = false;
   g_bUseDbgFlags       = false;
   g_csBadFileNameChars = '\/: *?"<>|';
   g_sRegKeyEmacs       = 'SOFTWARE\GNU\Emacs\';
   g_sRegKeyEmacsW32 = 'SOFTWARE\GNU\EmacsW32\';
   NLI                  = #13#10;

var
   g_bSkipAssoc          : boolean;
   g_bForAllUsers        : boolean;
   g_bForAllUsersDelHKCU : boolean;  { If writing to HKLM delete value in HKCU if true }
   g_iWriteToRegRoot     : integer;  { HKLM or HKCU }
   g_iCurrentRegRoot     : integer;  { where Emacs reg entries are now, HKLM or HKCU }
   g_teditTarGz          : TEdit;
   g_sEmacsTarGz         : String;
   g_bUnpackedEmacsTarGz : boolean;
   g_emacs_dir_ToInstall : string;  { emacs_dir to install now }
   g_emacs_dir_FromReg   : string;  { emacs_dir from registry }
   g_emacs_dir_FromPath  : string;  { emacs_dir from path }
   g_emacs_dir_Selected  : string;  { emacs_dir from user InputFile }
   g_bReboot             : boolean;
   g_bHomeNotSet         : boolean;
   g_sHomeDir            : string;
   g_sHomeHow            : string;
   g_sNewHomeDir         : string;
   g_sAPP                : string;  { replacement for app }
{    g_HomePageID        : integer; {\ = 1; !{\ }
{    g_SetHomePageID     : integer; {\ = 2; !{\ }
   g_iCurPageID          : Integer; { Current page ID }
   g_AppDirPageID        : integer; { = 3; }
   g_SelEmacsPageID      : integer; { = 3; }
   g_TarGzPageID         : integer; { = 4; }
   g_VanillaPageID       : integer; { = 5; }
   g_FTypePageID         : integer;
   g_ErrPageID           : integer;
   g_sErrFin             : string;
   g_lblErrFin           : TNewStaticText;

{ Note: I get an error on .add if I try to use TStrings instead of TStringList  }



{ /////////////////////////////////////////////////////////
//// Messages etc }


function myassert(bTrue : boolean ; sMsg : string ): boolean;
begin
   if not bTrue then begin
      msgbox(sMsg, mbCriticalError, MB_OK);
      Result := false;
   end else begin
      Result := true;
   end;
end; { myassert }


{ ToDo: Add error handlers to all these message functions that expand constants! }



{ ------------ Debug messages ----------- }

procedure MsgBoxTextVal(sName : String; sText : String);
begin
   if not g_bUseDbgFlags then exit;
   MsgBox(sName+' = ('+sText+')', mbInformation, MB_OK);
end; { DumpTextVal }

procedure MsgBoxDbg(sMsg : String);
begin
   if not g_bUseDbgFlags then exit;
   MsgBox(sMsg, mbInformation, MB_OK);
end; { MsgBoxDbg }

procedure MsgBox2(sMsg : String);
begin
   MsgBox(sMsg, mbInformation, MB_OK);
end; { MsgBoxDbg }
function MsgQuery(const sQueryName : string; const iButtons : integer): integer;
begin
   result := MsgBox(ExpandConstant('{cm:'+sQueryName+'}'), mbConfirmation, iButtons);
end; { MsgQuery }


{ ------------ Error and warning messages ----------- }

procedure MsgWarning(const sWarnName    : string);
begin
   MsgBox(ExpandConstant('{cm:'+sWarnName+'}'), mbError, MB_OK);
end; { MsgWarning }

procedure MsgCritical(const sErrName: string);
begin
   MsgBox(ExpandConstant('{cm:'+sErrName+'}'), mbCriticalError, MB_OK);
end; { MsgCritical }

function MyExpandConstEx(const sErrName, sValue : string): string;
var
   sMsg : string;
begin
   sMsg := ExpandConstant('{cm:'+sErrName+'}');
   StringChange(sMsg, '{value}', sValue);
   result := sMsg;
end; { MyExpandConstEx }

procedure MsgCriticalEx(const sErrName, sValue : string);
begin
   MsgBox(MyExpandConstEx(sErrName, sValue), mbCriticalError, MB_OK);
end; { MsgCriticalEx }

function MyExpandConstEx2(const sErrName, sVal1, sVal2 : string): string;
var
   sMsg : string;
begin
   sMsg := ExpandConstant('{cm:'+sErrName+'}');
   StringChange(sMsg, '{val1}', sVal1);
   StringChange(sMsg, '{val2}', sVal2);
   result := sMsg;
end; { MyExpandConstEx2 }

procedure MsgCriticalEx2(const sErrName, sVal1, sVal2 : string);
begin
   MsgBox(MyExpandConstEx2(sErrName,sVal1,sVal2), mbCriticalError, MB_OK);
end; { MsgCriticalEx2 }






{ ------------ Fail errors ----------- }

procedure AddToErrFin(const sErr : string);
begin
   { Just raise an exception for now - there was some strange behavior for early errors. }
   RaiseException(sErr);
   if (Length(g_sErrFin) > 0) then begin
      g_sErrFin := g_sErrFin + #13#10;
   end;
   g_sErrFin := g_sErrFin + sErr;
end; { AddToErrFin }



procedure CriticalError(const sErrName: string);
begin
   AddToErrFin(ExpandConstant('{cm:'+sErrName+'}'));
   MsgCritical(sErrName);
end; { CriticalError }

procedure CriticalErrorEx(const sErrName, sValue : string);
var
   sMsg : string;
begin
   sMsg := MyExpandConstEx(sErrName, sValue);
   AddToErrFin(sMsg);
   MsgCritical(sMsg);
end; { CriticalErrorEx }

procedure CriticalErrorEx2(const sErrName, sVal1, sVal2 : string);
var
   sMsg : string;
begin
   sMsg := MyExpandConstEx2(sErrName, sVal1, sVal2);
   AddToErrFin(sMsg);
   MsgCritical(sMsg);
end; { CriticalErrorEx2 }



procedure InternalError(sErrMsg : string);
var
   sMsg : string;
begin
   sMsg := ExpandConstant('{cm:InternalError}')
   +#13#10#13#10
   +sErrMsg
   +#13#10#13#10
   +ExpandConstant('{cm:PleaseReport}')
   AddToErrFin(sMsg);
   MsgBox(sMsg, mbCriticalError, MB_OK);
end; { InternalError }

procedure InternalErrorWithCode(iRes  : Integer; sMoreInfo : string);
begin
   InternalError('SysError:'+#13#10+SysErrorMessage(iRes)
                 +#13#10
                 +sMoreInfo);
end; { InternalErrorWithCode }






function AppFile(sSubFilePath : string): string; forward;


{ /////////////////////////////////////////////////////////
//// Utilities }
{ /////////////////////////////////////////////////////////}

function CompareVer(const vA1, vA2, vA3, vA4, vB1, vB2, vB3, vB4 : cardinal): integer;
begin
  result := 0;
  if vA1 = vB1 then begin
    if vA2 = vB2 then begin
      if vA3 = vB3 then begin
        if vA4 = vB4 then begin
        end else begin
          if vA4 > vB4 then result := 1 else result := -1;
        end;
      end else begin
        if vA3 > vB3 then result := 1 else result := -1;
      end;
    end else begin
      if vA2 > vB2 then result := 1 else result := -1;
    end;
  end else begin
    if vA1 > vB1 then result := 1 else result := -1;
  end;
end; { CompareVer }

function UXName(const sFileName: String): String;
begin
   StringChange(sFileName, '\', '/');
   Result := sFileName;
end;

function QS(const sStr: String): String;
begin
   Result := ' "' + sStr + '" ';
end;

function QUX(const sFileName: String): String;
begin
   Result := QS(UXName(sFileName));
end;



function GetBinVer(    const sFileName                  : string;
                   var verMaj, verMin, verRev1, verRev2 : cardinal): boolean;
var
   verMS : cardinal;
   verLS : cardinal;
begin
   result := GetVersionNumbers(sFileName, verMS, verLS);
   if result then begin
      //msgbox2(inttostr(verMS)+'  '+inttostr(verLS));
      verMin := verMS mod 65536;
      verMaj := (verMS - verMin) / 65536;
      verRev2 := verLS mod 65536;
      verRev1 := (verLS - verRev2) / 65536;
      //msgbox2(inttostr(verMaj)+'  '+inttostr(verMin) +'  ' +inttostr(verRev1)+'  '+inttostr(verRev2) );
   end;
end; { GetBinVer }

function VerToStr(const v1, v2, v3, v4 :  cardinal): string;
begin
   result := IntToStr(v1)+'.'+IntToStr(v2)+'.'+IntToStr(v3)+'.'+IntToStr(v4);
end; { VerToStr }

function VerIsMin22_0_50_1(): boolean; forward;



{*****************************************************}
{ Function to avoid misspelling of user selections... }

function DidSel(sWhat : string): boolean;
var
   tstrlist : TStringList;
begin
   tstrlist := TStringList.Create();
   tstrlist.CommaText := WizardSelectedComponents(false);
   Result := (tstrlist.IndexOf(sWhat) <> -1);
end; { DidSel }

function DidSelEmacsItself(): boolean;
begin
   result := false
   if g_bIncludesEmacs then begin
      result := DidSel('emacsitself');
   end;
end; { DidSelEmacsItself }

function DidSelUnpacking(): boolean;
begin
   if g_bIncludesEmacs then begin
      result := false
   end else begin
      Result := DidSel('unpack');
   end;
end; { DidSelUnpacking }

function DidSelBasicSetup(): boolean;
begin
   result := DidSel('base');
end; { DidSelBasicSetup }

function BasicAndNotSilent(): boolean;
begin
   result := (not WizardSilent()) and DidSelBasicSetup();
end; { BasicAndNotSilent }

function DidSelWinShcut(): boolean;
begin
   result := DidSel('win\winshcut');
end; { DidSelWinShcut }

function DidSelAssoc(): boolean;
begin
   result := DidSel('win\assoc');
end; { DidSelAssoc }

{ function DidSelHomeTask(): boolean; }
{ begin }
{    result := DidSel('win\home'); }
{ end; {\ DidSelHomeTask !{\ }

function DidSelWebEditIE(): boolean;
begin
   result := DidSel('win\web-edit\ie');
end; { DidSelWebEditIE }

function DidSelAnyCustomization(): boolean;
begin
   result :=
   DidSelWinShcut()
   or DidSelAssoc()
   or DidSelWebEditIE()
   ;
end; { DidSelAnyCustomization }











{ Reg functions for HKLM or HKCU }

function ULRegKeyExists(const sSubKeyName: string; var iRoot : integer): boolean;
begin
   result := true;
   if RegKeyExists(HKCU, sSubKeyName) then begin
      iRoot := HKCU;
   end else if RegKeyExists(HKLM, sSubKeyName) then begin
      iRoot := HKLM;
   end else begin
      result := false;
   end;
end; { ULRegKeyExists }

function ULRegValueExists(    const sSubKeyName, sValueName : string;
                          var iRoot                         : integer): boolean;
begin
   result := false;
   if RegValueExists(HKCU, sSubKeyName, sValueName) then begin
      iRoot := HKCU;
      result := true;
   end else if RegValueExists(HKLM, sSubKeyName, sValueName) then begin
      iRoot := HKLM;
      result := true;
   end;
end; { ULRegValueExists }


function ULRegQueryStringValue(const sSubKeyName,sValName : string;var sRes : string): boolean;
begin
   result := false;
   if RegQueryStringValue(HKCU, sSubKeyName, sValName, sRes) then begin
      result := true;
   end else if RegQueryStringValue(HKLM, sSubKeyName, sValName, sRes) then begin
      result := true;
   end else begin
   end;
end; { ULRegQueryStringValue }

function ULRegGetSubkeyNames(const sSubKeyName : string;
                             var tasNames : TArrayOfString): boolean;
var
   sTrace : string;
begin
   result := false;
   if RegGetSubkeyNames(HKCU, sSubKeyName, tasNames) then begin
      if GetArrayLength(tasNames) > 0 then begin
         result := true;
         sTrace := 'HKCU';
      end;
   end;
   if not result then begin
      if RegGetSubkeyNames(HKLM, sSubKeyName, tasNames) then begin
         if GetArrayLength(tasNames) > 0 then begin
            result := true;
            sTrace := 'HKLM';
         end;
      end;
   end;
end; { ULRegGetSubkeyNames }


function ULRegWriteStringValue(const sSubKeyName, sValueName, sValue : string): boolean;
begin
   if g_bForAllUsersDelHKCU then begin
      { MsgBox2('here'); }
      RegDeleteValue(HKCU, sSubKeyName, sValueName);
   end;
   { MsgBox2('g_iWriteToRegRoot='+inttostr(g_iWriteToRegRoot)+', key, nam, val='+ sSubKeyName+', '+sValueName+', '+sValue); }
   result := RegWriteStringValue(g_iWriteToRegRoot, sSubKeyName, sValueName, sValue);
end; { ULRegWriteStringValue }

function iRegRoot2s(iRegRoot : integer):string;
begin
   result:= '';
   case iRegRoot of
     HKLM : begin result := 'HKLM'; end;
     HKCU : begin result := 'HKCU'; end;
     HKCR : begin result := 'HKCR'; end;
   end; { case }
end; { iRegRoot2s }




function IsBasicSetupDoneBefore(): boolean;
begin
   //result := FileExists(ExpandConstant(AppFile('EmacsW32/bin/gnuserv.exe')));
   //msgbox2(g_emacs_dir_FromReg+'#');
   //msgbox2(g_emacs_dir_FromReg+'\..\EmacsW32/bin/gnuserv.exe');
   //msgbox2(ExpandFileName(g_emacs_dir_FromReg+'/../EmacsW32/bin/gnuserv.exe'));
   result := FileExists(ExpandFileName(g_emacs_dir_FromReg+'/../EmacsW32/bin/gnuserv.exe'));
end; { IsBasicSetupDoneBefore }



{ ********************************************* }
{ Scripted constants support functions }
{ ********************************************* }


function emacs_dir_ForSelect(): string;
begin
   if length(g_emacs_dir_FromReg) > 0 then begin
      result := g_emacs_dir_FromReg;
   end else if length(g_emacs_dir_FromPath) > 0 then begin
      result := g_emacs_dir_FromPath;
   end else begin
      result := '';
   end;
end; { emacs_dir_ForSelect }

function emacs_dir_Known(): string;
begin
   //msgbox2('emacs_dir_Known');
   { if g_bIncludesEmacs then begin }
   if g_bIncludesEmacs then begin
      if DidSelEmacsItself() then begin
         InternalError('g_bIncludesEmacs is true');
      end else begin
         result := g_emacs_dir_Selected;
      end;
   end else if DidSelUnpacking() then begin
      result := g_emacs_dir_ToInstall;
   end else if length(g_emacs_dir_Selected) > 0 then begin
      result := g_emacs_dir_Selected;
   end else begin
      result := emacs_dir_ForSelect();
   end;
end; { emacs_dir_Known }

function emacs_dir_ToUse(): string;
begin
   //msgbox2('emacs_dir_ToUse');
   result := emacs_dir_Known();
   if length(result) = 0 then begin
      InternalError('Can not find emacs_dir to use');
   end;
end; { emacs_dir_ToUse }


function emacs_dir_ForCode(sSubdir : string): string;
begin
   //msgbox2('emacs_dir_ForCode');
   result := emacs_dir_ToUse();
   if length(sSubDir) > 0 then begin
      result := result + '\' + sSubDir;
   end;
end; { emacs_dir_ForCode }


function GetEmacsBinVer(var v1, v2, v3, v4 : cardinal): boolean; forward;

function Check_emacs_dir_Ok()                           : boolean;
var
   sParentDir   : string;
   sEmacsSubDir : string;
   tstrsDirs    : TStringList;
   tstrsFiles   : TStringList;
   sFile        : string;
   nKnownDirs   : integer;
   nKnownFiles  : integer;
   sMsg         : string;
   ss           : string;
   FindRec      : TFindRec;
   verMaj  : cardinal;
   verMin  : cardinal;
   verRev1 : cardinal;
   verRev2 : cardinal;
begin
   result := false;
   if not VerIsMin22_0_50_1() then begin
      if GetEmacsBinVer(verMaj, verMin, verRev1, verRev2) then begin
         sMsg := 'Emacs must be version 22.0.50.1 or above.'
         +' The version of emacs.exe in the directory you have choosen seems to be '
         +IntToStr(verMaj)+'.'+IntToStr(verMin)+'.'+IntToStr(verRev1)+'.'+IntToStr(verRev2)+'.'
         +#13#10#13#10
         +'There is however currently a bug in Emacs version numbers in emacs.exe on MS Windows.'
         +' If you are sure you are using Emacs 22.0.50.1 or later, then please continue.'
         +' You can check this inside Emacs with M-x version RET.'
         +#13#10#13#10
         +' Do you want to continue?'
         ;
         if (MsgBox(sMsg, mbConfirmation, MB_YESNO) = IDNO) then exit;
      end else begin
         msgbox2('Can not get Emacs version. You are probably using a too old version of Emacs.');
      end;
   end;

   result := false;
   { if g_bIncludesEmacs or DidSelUnpacking() then begin }
   if DidSelEmacsItself() or DidSelUnpacking() then begin
      result := true;
      exit;
   end;
   if not myassert(length(emacs_dir_Known()) > 0, 'emacs_dir unknown in Check_emacs_dir_Ok')
      then exit;
   sParentDir := AddBackSlash(ExtractFileDir(emacs_dir_Known()));
   sEmacsSubDir := ExtractFileName(emacs_dir_Known());
   tstrsDirs := TStringList.Create();
   tstrsFiles := TStringList.Create();

   //sFile := FindFirst(sParentDir+'*', FindRec);
   if FindFirst(sParentDir+'*', FindRec) then begin
      try
         repeat
            sFile := FindRec.Name;
            if FindRec.Attributes and FILE_ATTRIBUTE_DIRECTORY <> 0 then begin
               if (sFile <> '.') and (sFile <> '..') then begin
                  if (sFile = 'EmacsW32') or
                     (sFile = 'site-lisp') or
                     (sFile = sEmacsSubDir)
                     then nKnownDirs := nKnownDirs+1
                  else tstrsDirs.Add(sFile);
               end;
            end else begin
               if ('emacs.cmd' = sFile) or
                  (Copy(sFile, 1, 5) = 'unins') or
                  ('NotReadme.txt' = sFile)
                  then nKnownFiles := nKnownFiles+1
               else tstrsFiles.Add(sFile);
            end;
         until not FindNext(FindRec);
         finally
         FindClose(FindRec);
      end;
      if tstrsDirs.Count + tstrsFiles.Count = 0 then begin
         result := true;
         exit;
      end;
      {
      for nn:=0 to tstrsFiles.Count-1 do begin sFile := tstrsFiles.Strings[nn]; end;
      for nn:=0 to tstrsDirs.Count-1 do begin sSubDir := tstrsDirs.Strings[nn]; end;
      }
      sMsg :=
      'WARNING - There are unknown files and/or directories in the installation directory.'
      +#13#10#13#10;
      //+sParentDir+'):'
      ss := tstrsFiles.CommaText; StringChange(ss,',', ', ');
      if length(ss) > 0 then begin
         sMsg := sMsg+'  ** Unknown files:   '+ss +#13#10#13#10;
      end;
      ss := tstrsDirs.CommaText; StringChange(ss, ',', ', ');
      if length(ss) > 0 then begin
         sMsg := sMsg+'  ** Unknown directories:   '+ss
      end;
      sMsg := sMsg
      +#13#10#13#10
      +#13#10#13#10
      +'The installation directory is "'+sParentDir+'". '
      +'Some directories and files will be added there. '
      +#13#10#13#10
      +'Are you sure that you want to use this directory?';
      result := (MsgBox(sMsg, mbConfirmation, MB_YESNO) = IDYES);
   end;
end; { Check_emacs_dir_Ok }

function AppSubDir(sDefault : string): string;
begin
   { if g_bIncludesEmacs then begin }
   if DidSelEmacsItself() then begin
      result := AddBackSlash(ExpandConstant('{app}')) + sDefault;
   end else begin
      { app constant can not be used because of unpacking }
      //msgbox2('appsubdir');
      result := AddBackSlash(ExtractFileDir(emacs_dir_ToUse())) + sDefault;
      //msgbox2('appsubdir2 '+sDefault);
   end;
end; { AppSubDir }

function AppSubDir2(sDefault : string): string;
begin
   //result := '"'+AppSubDir(sDefault)+'"';
   result := AppSubDir(sDefault);
end; { AppSubDir2 }

function AppFile(sSubFilePath : string): string;
begin
   result := RemoveBackSlash(AppSubDir(sSubFilePath));
end; { AppFile }

function EmacsIsInstalled(): boolean;
begin
   result := length(g_emacs_dir_FromReg) > 0;
end; { EmacsIsInstalled }


function DirToInstallEmacsItself(): string;
begin
   if length(g_emacs_dir_ToInstall) = 0 then begin
      InternalError('g_emacs_dir_ToInstall not set in DirToInstallEmacsItself');
      result := 'missing g_emacs_dir_ToInstall';
      exit;
   end;
   result := AddBackSlash(g_emacs_dir_ToInstall);
end; { DirToInstallEmacsItself }

function OpenAction(sDefault:string): string;
begin
   result := AppFile('Emacs\bin\emacsclientw.exe') + ' -n "%1"';
end; { OpenAction }








{ /////////////////////////////////////////////////////////
//// Finding the installed Emacs }

{ If admin and the Emacs reg key is for user we have a problem! }
{ This actually does a bit too much, but it is still reasonable I believe! }
function ULEmacsConflict(): boolean;
var
   iRoot : integer;
begin
   result := false;
   if IsAdminLoggedOn() then begin
      if ULRegKeyExists(g_sRegKeyEmacs, iRoot) then begin
         result := (iRoot <> HKLM);
      end;
   end;
end; { ULEmacsConflict }

{ Actually this shold be done for every user... }
function DeleteHKCUEmacsKey(): boolean;
begin
   result := false;
   if not RegDeleteValue(HKCU, g_sRegKeyEmacs, 'emacs_dir') then exit;
   {
   if not RegDeleteValue(HKCU, g_sRegKeyEmacs, 'EMACSDATA') then exit;
   if not RegDeleteValue(HKCU, g_sRegKeyEmacs, 'EMACSDOC') then exit;
   if not RegDeleteValue(HKCU, g_sRegKeyEmacs, 'EMACSLOADPATH') then exit;
   if not RegDeleteValue(HKCU, g_sRegKeyEmacs, 'EMACSLOCKDIR') then exit;
   if not RegDeleteValue(HKCU, g_sRegKeyEmacs, 'EMACSPATH') then exit;
   if not RegDeleteValue(HKCU, g_sRegKeyEmacs, 'SHELL') then exit;
   if not RegDeleteValue(HKCU, g_sRegKeyEmacs, 'TERM') then exit;
   if not RegDeleteKeyIfEmpty(HKCU, g_sRegKeyEmacs) then exit;
   }
   result := true;
end; { DeleteHKCUEmacsKey }



function UnpackEmacsTarGz() : Boolean; forward;

function GetEmacsBinVer(var v1, v2, v3, v4 : cardinal): boolean;
var
   sEmacsExe : string;
begin
   result := false;
   { if g_bIncludesEmacs then begin }
   if DidSelEmacsItself() then begin
      try
         ExtractTemporaryFile('runemacs.exe');
      except
         CriticalErrorEx2('FailedExtractTmp', 'runemacs.exe', GetExceptionMessage());
         exit;
      end;
      sEmacsExe := AddBackSlash(ExpandConstant('{tmp}'))+'runemacs.exe';
      if not FileExists(sEmacsExe) then begin
         CriticalErrorEx('ExtrTmpNotFound', sEmacsExe);
         exit;
      end;
   end else begin
      { extract files here if not done! }
      if DidSelUnpacking() then begin
         if not g_bUnpackedEmacsTarGz then begin
            if not UnpackEmacsTarGz()then begin
               exit;
            end;
         end;
      end;
      sEmacsExe := emacs_dir_ToUse() + '\bin\runemacs.exe';
      MsgBoxDbg(sEmacsExe);
   end;
   if not GetBinVer(sEmacsExe, v1, v2, v3, v4) then begin
      Msgbox2('could not get version from '+sEmacsExe);
   end else begin
      result := true;
   end;
end; { GetEmacsBinVer }

function VerIsMin22_0_50_1(): boolean;
var
   verMaj  : cardinal;
   verMin  : cardinal;
   verRev1 : cardinal;
   verRev2 : cardinal;
   iComp   : integer;
begin
   result := false;
   if GetEmacsBinVer(verMaj, verMin, verRev1, verRev2) then begin
      { According to Jason the last number does not matter so I compare with 0 here }
      iComp := CompareVer(verMaj, verMin, verRev1, verRev2, 22, 0, 50, 0);
      { msgbox2(VerToStr(verMaj, VerMin, verRev1, verRev2)+'\n'+IntToStr(iComp)); }
      result := (iComp >= 0);
   end;
end; { VerIsMin22_0_50_1 }



function GetEmacsDirFromReg(): boolean;
var
   sRes : string;
   sExe : string;
begin
   result := false;
   g_emacs_dir_FromReg := '';
   { if not ULRegKeyExists(g_sRegKeyEmacs, g_iCurrentRegRoot) then exit; }
   if not ULRegValueExists(g_sRegKeyEmacs, 'emacs_dir', g_iCurrentRegRoot) then exit;
   if not ULRegQueryStringValue(g_sRegKeyEmacs, 'emacs_dir', sRes) then exit;
   sExe := RemoveBackslash(sRes)+'\bin\emacs.exe';
   if not FileExists(sExe) then begin
{       CriticalErrorEx2('RegFileNotFound', sExe, }
      MsgCriticalEx2('RegFileNotFound', sExe,
                     iRegRoot2s(g_iCurrentRegRoot)+'\'+g_sRegKeyEmacs);
   end else begin
      g_emacs_dir_FromReg := RemoveBackSlash(sRes);
      { g_sAPP := ExtractFileDir(g_emacs_dir_FromReg); }
      result := true;
   end;
end; { GetEmacsDirFromReg }

function GetEmacsDirFromPath(): boolean;
var
   sEmacsExe : string;
begin
   { msgbox2('GetEmacsDirFromPath'); }
   result := false;
   { if not myassert(length(emacs_dir_Known())=0, 'emacs_dir known but called GetEmacsDirFromPath') then exit; }
   sEmacsExe := FileSearch('emacs.exe', GetEnv('PATH'));
   if length(sEmacsExe) = 0 then exit;
   g_emacs_dir_FromPath := ExtractFileDir(ExtractFileDir(sEmacsExe));
   result := true;
end; { GetEmacsDirFromPath }













function ForAllUsers(): Boolean;
begin
   result := g_bForAllUsers;
end; { ForAllUsers }
function ForCurrentUser(): Boolean;
begin
   result := not g_bForAllUsers;
end; { ForCurrentUser }


function CanWriteHKLM(): Boolean;
begin
   result := IsAdminLoggedOn();
end; { CanWriteHKLM }
function IsUser(): boolean;
begin
   //result := not CanWriteHKLM();
   result := not IsAdminLoggedOn();
end; { IsUser }

function InstalledAndCanWriteHKLM(): boolean;
begin
   result := (EmacsIsInstalled() and CanWriteHKLM());
end; { InstalledAndCanWriteHKLM }
function NotInstalledAndCanWriteHKLM(): boolean;
begin
   result := ((Not EmacsIsInstalled()) and CanWriteHKLM());
end; { NotInstalledAndCanWriteHKLM }

function BaseAndIsAdmin(): boolean;
begin
   result := (IsAdminLoggedOn() and DidSelBasicSetup());
end; { BaseAndIsAdmin }
function BaseAndIsUser(): boolean;
begin
   result := (IsUser() and DidSelBasicSetup());
end; { BaseAndIsUser }

function InstalledAndIsUser(): boolean;
begin
   result := (EmacsIsInstalled() and IsUser());
end; { InstalledAndIsUser }

function NotInstalledAndIsUser(): boolean;
begin
   result := ((Not EmacsIsInstalled()) and IsUser());
end; { NotInstalledAndIsUser }

{ function HasNoHome(): boolean; }
{ begin }
{    result := g_bHomeNotSet; }
{    result := false; {\ not needed for Emacs 22 and probably few 21.3 users are using this now !{\ }
{ end; {\ HasNoHome !{\ }




function MsgboxFileExists(sFileName: String): Boolean;
begin
   Result := false;
   if not FileExists(sFileName) then begin
      MsgBox('File does not exist: "'+sFileName+'"', mbInformation, MB_OK);
      exit;
   end;
   Result := true;
end; { MsgboxFileExists }


function CopyDir(const sExistingDir, sNewDir : string; const bMayExist : boolean): boolean;
var
   tstrsDirs  : TStringList;
   tstrsFiles : TStringList;
   sFile      : string;
   sSubDir    : string;
   nn         : integer;
   sFrom      : string;
   sTo        : string;
   FindRec    : TFindRec;
begin
   tstrsDirs := TStringList.Create();
   tstrsFiles := TStringList.Create();
   sExistingDir := AddBackSlash(sExistingDir);
   sNewDir := AddBackSlash(sNewDir);
   if FileOrDirExists(sNewDir) then begin
      if not bMayExist then begin
         InternalError('Something is wrong, dir exists: '+sNewDir);
         exit;
      end else begin
         CreateDir(sNewDir);
      end;
   end;
   if FindFirst(sExistingDir+'*', FindRec) then begin
      try
         repeat
            sFile := FindRec.Name;
            if FindRec.Attributes and FILE_ATTRIBUTE_DIRECTORY <> 0 then begin
               if (sFile <> '.') and (sFile <> '..') then tstrsDirs.Add(sFile);
            end else begin
               tstrsFiles.Add(sFile);
            end;
         until not FindNext(FindRec);
         finally
            FindClose(FindRec);
      end;
   end;
   for nn:=0 to tstrsDirs.Count-1 do begin
      sSubDir := tstrsDirs.Strings[nn];
      if not CopyDir(sExistingDir+sSubDir, sNewDir+sSubDir, bMayExist) then exit;
   end;
   for nn:=0 to tstrsFiles.Count-1 do begin
      sFile := tstrsFiles.Strings[nn];
      sFrom := sExistingDir+sFile;
      sTo := sNewDir+sFile;
      if not FileCopy(sFrom, sTo, true) then begin
         Msgbox2('Failed copying '+sFrom+' -> '+sTo);
      end;
   end;
   result := true;
end; { CopyDir }




function RunCmd(sExecFile : String; sParam : String; sWorkDir: String): Boolean;
var
   iRes      : Integer;
   sShellCmd : String;
   sCmdFlag  : string;
begin
   Result := false;
   //MsgBox2('RunCmd('+sExecFile+','+#13#10+sParam+','+#13#10+sWorkDir+')');
   if sExecFile <> '' then begin
      if not MsgboxFileExists(sExecfile) then begin
         exit;
      end;
   end;
   sShellCmd := sExecFile+' '+sParam;
   MsgBoxTextVal('sShellCmd', sShellCmd);
   if g_bUseDbgFlags then begin
      sCmdFlag := ' /k ';
   end else begin
      sCmdFlag := ' /c ';
   end;
   if not Exec(
               'cmd.exe',
               sCmdFlag+sShellCmd,
               sWorkDir,
               SW_SHOW,
               ewWaitUntilTerminated,
               iRes) then begin
                  InternalErrorWithCode(iRes, '(from RunCmd: '+sShellCmd
                                        +', in dir '+sWorkDir+')');
                  exit;
               end;
   MsgBoxDbg('RunCmd OK');
   Result := true;
end;

function RunFile(sExecFile : String; sParam : String; sWorkDir : String): Boolean;
var
   iRes              : Integer;
begin
   Result := false;
   if true then begin
      if not MsgboxFileExists(sExecfile) then begin
         exit;
      end;
      //MsgBoxDbg('RunFile'+#13#10+'sExecFile='+sExecfile+#13#10+'sParam='+sParam);
      if not Exec(
                  sExecFile,
                  sParam,
                  sWorkDir,
                  SW_SHOW,
                  ewWaitUntilTerminated,
                  iRes) then begin
                     InternalErrorWithCode(iRes, '(from RunFile: '
                                           +sExecFile+' '+sParam
                                           +' in dir '+sWorkDir+')');
                     exit;
                  end;
      Result := true;
   end else begin
      Result := RunCmd(sExecFile, sParam, sWorkDir);
   end;
end; { RunFile }













{ ///////////////////////////////////////////////////////// }
{ //// Emacs Home (The idea and much of the code are from Frank Schmitt) }

const g_sHOMEREGPATH = 'SOFTWARE\GNU\Emacs';
function FindEmacsUserHome(var sHome, sHow :string): Boolean;
{ According do Emacs FAQ For Windows 95/98/ME/NT/XP and 2000 you
should first look for the env variable HOME and then look in the
registry keys (in this order)

HKEY_CURRENT_USER\\SOFTWARE\\GNU\\Emacs
HKEY_LOCAL_MACHINE\\SOFTWARE\\GNU\\Emacs <-should be avoided since this is not per user!

for a value named HOME. }
begin
   sHome := GetEnv('HOME');
   if (sHome <> '') then begin
      sHow := 'ENV';
   end else begin
      result := ULRegQueryStringValue(g_sHOMEREGPATH, 'HOME', sHome);
   end;
   Result := (sHome <> '');
end; { FindEmacsUserHome }

function AddHomeToEnv(Home: String): boolean;
begin
   result := RegWriteStringValue(HKCU, 'Environment', 'HOME', Home);
end; { AddHomeToEnv }

function NeedRestart(): boolean;
begin
   result := g_bReboot;
end; { NeedRestart }

function CreateHomeDir(Home: String): boolean;
begin
   if DirExists(Home) then Result := true else Result := ForceDirectories(Home);
end; { CreateHomeDir }

function IsCorrectDir(sDir: String): boolean;
begin
   if (Length(sDir)<4) then begin
      result := false
   end else begin
      if ((StrGet(sDir,2) = ':') and (StrGet(sDir,3) = '\')) then begin
         result := true
      end else begin
         if ((StrGet(sDir,1) = '\') and (StrGet(sDir,2) = '\')) then begin
            result := true;
         end;
      end;
   end;
end; { IsCorrectDir }
















{ /////////////////////////////////////////////////////////
//// Finding and unpacking distribution file }


function BrowseForEmacsTarGz(const sInitDir : String): Boolean;
var
   sFile : string;
   bRes  : boolean;
begin
   Result := false;
   bRes := GetOpenFileName(
                           'Select Emacs binary distribution file (type *.tar.gz)',
                           sFile,
                           sInitDir,
                           'Tar GZ file (*.tar.gz)|*.gz',
                           '.gz');
   if bRes then begin
      Result := bRes;
      g_sEmacsTarGz := sFile;
   end;
end; { BrowseForEmacsTarGz }

procedure WriteRegEmacsTarGz();
begin
   //msgbox2('w '+g_sRegKeyEmacsW32);
   if not RegWriteStringValue(HKCU, g_sRegKeyEmacsW32,
                              'LastEmacsTarGzFile', g_sEmacsTarGz) then begin
      InternalError('WriteRegEmacsTarGz failed');
   end;
end; { WriteRegEmacsTarGz }

function ReadRegEmacsTarGz(): String;
var
   sRes  : String;
   iRoot : integer;
begin
   Result := '';
   //msgbox2('r '+g_sRegKeyEmacsW32);
   if ULRegKeyExists(g_sRegKeyEmacsW32, iRoot) then begin
      if ULRegValueExists(g_sRegKeyEmacsW32, 'LastEmacsTarGzFile', iRoot) then begin
         if ULRegQueryStringValue(g_sRegKeyEmacsW32, 'LastEmacsTarGzFile', sRes) then begin
            MsgBoxTextVal('ReadReg sRes', sRes);
            Result := sRes;
         end else begin
            InternalError('ReadRegEmacsTarGz failed');
         end;
      end;
   end;
end; { ReadRegEmacsTarGz }


function CheckTarGz(const sTarGz : string): Boolean;
var
   sTail : String;
begin
   Result := false;
   MsgBoxTextVal('CheckTarGz, sTarGz=', sTarGz);
   if sTarGz = '' then begin
      MsgBox('Please enter the path to GNU Emacs binary distribution file for MS Windows',
             mbInformation, MB_OK);
      exit;
   end;
   if not MsgboxFileExists(sTarGz) then begin
      exit;
   end;
   sTail := Copy(sTarGz, length(sTarGz)-7+1, 100);
   if not myassert(length(sTail) = 7, 'Length of sTail is not 6') then exit;
   if (CompareText(sTail, '.tar.gz') <> 0) then begin
      MsgBoxTextVal('sTail', sTail);
      MsgCritical('MustEndInTarGz');
      exit;
   end;

   Result := true;
end; { CheckTarGz }



const
   S7ZIPREGKEY = 'SOFTWARE\7-Zip';

function Get7zExe(var s7zExe : string): boolean;
var
   iRoot : integer;
begin
   result := false;
   if ULRegValueExists(S7ZIPREGKEY, 'Path', iRoot) then begin
      if ULRegQueryStringValue(S7ZIPREGKEY, 'Path', s7zExe) then begin
         s7zExe := s7zExe + '\7za.exe';
      end;
   end;
   if (Length(s7zExe) = 0) or (not FileExists(s7zExe)) then begin
      s7zExe := AppFile('EmacsW32\bin\7za.exe');
   end;
   //s7zExe := 'no such file i hope...';
   if not FileExists(s7zExe) then begin
      { Need to extract 7-zip temporary since it has not been extracted yet! }
      { Note: This will be deleted automatically after install by Inno.      }
      try
         ExtractTemporaryFile('7za.exe');
      except
         CriticalErrorEx2('FailedExtractTmp', '7za.exe', GetExceptionMessage());
         exit;
      end;
      s7zExe := AddBackSlash(ExpandConstant('{tmp}'))+'7za.exe';
      if not FileExists(s7zExe) then begin
         CriticalErrorEx('ExtrTmpNotFound', s7zExe);
         exit;
      end;
   end;
   result := true;
end; { Get7zExe }


function FindFreeTempFileName(const sBaseName : string): string;
var
   sFreeName     : string;
   nn            : integer;
begin
   sFreeName := sBaseName;
   result := '';
   while nn < 100 do begin
      if nn > 0 then sFreeName := sBaseName+'-'+IntToStr(nn);
      if FileOrDirExists(sFreeName) then begin
         nn := nn+1;
      end else begin
         result := sFreeName;
         exit;
      end;
   end; { while }
   CriticalErrorEx('CantFindFreeTempName', sBaseName);
end; { FindFreeTempFileName }


function UnpackTarGz(    const sTarGzFile           : string;
                         const sFinOutPath          : string;
                     var sOldOutPath                : string): Boolean;
var
   s7zExe         : String;
   sInFileName    : string;
   sParam         : String;
   sTmpTarName    : string;
   sTmpTarPath    : string;
   sTmpTarFile    : string;
   sTarOutPath    : string;
   sTarUnpackPath : string;
   nn             : integer;
   n2             : integer;
   sOldPrefix     : string;
begin
   Result := false;
   sFinOutPath := RemoveBackSlash(sFinOutPath);

   if not Get7zExe(s7zExe) then exit;
   { We can't for some reason use quotes around the exe so use the short name: }
   s7zExe := GetShortName(s7zExe);

   sInFileName := ExtractFileName(sTarGzFile);
   //msgbox2('sInFileName='+sInFileName);
   sTmpTarPath := ExpandConstant('{tmp}\tar\');
   //msgbox2('sTmpTarPath='+sTmpTarPath);
   sTmpTarName := sInFileName;
   StringChange(sTmpTarName, '.tar.gz', '.tar');
   //msgbox2('sTmpTarName='+sTmpTarName);
   sTmpTarFile := sTmpTarPath + sTmpTarName;
   //msgbox2('sTmpTarFile='+sTmpTarFile);
   sParam := s7zExe+' x -o"'+sTmpTarPath+'" "'+g_sEmacsTarGz+'"';
   if FileExists(sTmpTarFile) then begin
      CriticalErrorEx('TempTarAlreadyExists', sTmpTarFile);
      exit;
   end;
   if not RunCmd('', sParam, '.') then begin
      CriticalError('UnpackingTarGzFailed');
      exit;
   end;
   if not FileExists(sTmpTarFile) then begin
      CriticalErrorEx('TempTarNotFound', sTmpTarFile);
      exit;
   end;

   sTarUnpackPath := FindFreeTempFileName(sFinOutPath+'-tartmp');
   //msgbox2('sTarUnpackPath='+sTarUnpackPath);
   sParam := s7zExe+' x -o"'+sTarUnpackPath+'" "'+sTmpTarFile+'"';
   if not RunCmd('', sParam, '.') then begin
      CriticalError('UnpackingTarFailed');
      exit;
   end;
   sTarOutPath := sTmpTarName;
   StringChange(sTarOutPath, '.tar', '');

   { This is for Emacs naming conventions for tar files, like emacs-21.3-fullbin-i386.tar.gz }
   { where the tar member files are all in the subdir emacs-21.3                             }
   nn := Pos('-', sTarOutPath);
   n2 := Length(sTarOutPath);
   while (n2 > nn) do begin
      if (StrGet(sTarOutPath, n2) = '-') then begin
         SetLength(sTarOutPath, n2-1);
      end;
      n2 := n2 - 1;
   end;

   sTarOutPath := sTarUnpackPath+'/'+sTarOutPath;
   //msgbox2('sTarOutPath='+sTarOutPath);
   if not DirExists(sTarOutPath) then begin
      CriticalErrorEx('TarOutputNotFound', sTarOutPath);
      exit;
   end;

   if FileOrDirExists(sFinOutPath) then begin
      if length(sOldOutPath) = 0 then
         sOldPrefix := sFinOutPath+'-old' else sOldPrefix := sOldOutPath;
      sOldOutPath := FindFreeTempFileName(sOldPrefix);
      while not RenameFile(sFinOutPath, sOldOutPath) do begin
         if MsgBox('Could not rename "'+sFinOutPath+'" => "'+sOldOutPath+'"'
                   +#13#10
                   +#13#10
                   +'Did you exit emacs?'
                   +' Is there any other program running that could cause the problem?'
                   +' Press OK to try renaming again.',
                   mbCriticalError, MB_OKCANCEL) <> IDOK then begin
                      sOldOutPath := '';
                      exit;
                   end;
      end; { while }
   end else begin
      sOldOutPath := '';
   end;
   if not RenameFile(sTarOutPath, sFinOutPath) then begin
      CriticalErrorEx('RenameFailed', sTarOutPath+'" => "'+sFinOutPath);
      exit;
   end;
   //if MsgBox('remove '+sTarUnpackPath, mbConfirmation, MB_YESNO) = IDYES then begin
      if not RemoveDir(sTarUnpackPath) then begin
         CriticalErrorEx('RemoveTempTarDirFailed', sTarUnpackPath);
         exit;
      end;
   //end;

   //msgbox2('after rename');

   Result := True;
end; { UnpackTarGz }

function UnpackEmacsTarGz()                  : Boolean;
var
   sFinOutPath   : string;
   sOldOutPath   : string;
   sOldSiteLisp2 : string;
   sNewSiteLisp2 : string;
   sMsg          : string;
begin
   Result := false;
   if not myassert(DidSelUnpacking, 'unpacking Emacs but did not want to') then exit;
{
   sFinOutPath := ExtractFileName(g_sEmacsTarGz);
   nPos := Pos('-', sFinOutPath);
   SetLength(sFinOutPath, nPos-1);
}
   sFinOutPath := DirToInstallEmacsItself();
   //msgbox2('UnpackEmacsTarGz.sFinOutPath='+sFinOutPath);
   sOldOutPath := '';
   if UnpackTarGz(g_sEmacsTarGz, sFinOutPath, sOldOutPath) then begin
      sMsg := 'Unpacked g_sEmacsTarGz='+g_sEmacsTarGz
              +#13#10
              'sFinOutPath='+sFinOutPath
              +#13#10
              'sOldOutPath='+sOldOutPath
              ;
      //msgbox2(smsg);
      sOldSiteLisp2 := AddBackSlash(sOldOutPath)+'site-lisp';
      if DirExists(sOldSiteLisp2) then begin
         if not CopyDir(sOldSiteLisp2, sNewSiteLisp2, true) then begin
            CriticalError('RenameSiteLisp2Failed');
            exit;
         end;
      end;
      g_bUnpackedEmacsTarGz := true;
      Result := true;
   end else begin
      CriticalError('UnpackingTarGzFailed');
   end;
end; { UnpackEmacsTarGz }














function RunLispAfterInstall(sLispFile     : string;
                             bNeedDotEmacs : boolean ) : Boolean;
var
   sEmacsExe : String;
   sEmacsPar : String;
   sLisp     : AnsiString;
begin
   Result := false;
   { These files are not available yet! }
   sEmacsExe := emacs_dir_ToUse() + '\bin\emacs.exe';
   //msgbox2('runlispaafterinstall sEmacsExe='+sEmacsExe);
   if not MsgboxFileExists(sLispFile) then exit;
   if not MsgboxFileExists(sEmacsExe) then begin
      CriticalError('DefaultElNoEmacsExe');
      exit;
   end;
   if bNeedDotEmacs then begin
      //sEmacsPar := ' -nw -l "'+sLispFile+'" -f kill-emacs ';
      sEmacsPar := ' -nw -l "'+sLispFile+'"';
   end else begin
      sEmacsPar := ' -batch -l "'+sLispFile+'"';
   end;
   LoadStringFromFile(sLispFile, sLisp);
   { msgbox2('RunLispAI, sLispFile='+sLispFile
           +#13#10
           +#13#10
           +sLisp);
   }
   if g_bUseDbgFlags then begin
      result := RunCmd(sEmacsExe, sEmacsPar, AppSubDir(''));
   end else begin
      result := RunFile( sEmacsExe, sEmacsPar, AppSubDir(''));
   end;
   if not result then InternalError('Failed executing '+sEmacsExe+sEmacsPar);
end; { RunLispAfterInstall }

function LispSetupBase(): boolean;
var
   sLispFile : string;
begin
   sLispFile := AppFile('EmacsW32\lisp\emacsw32-setup-base.el');
   result := RunLispAfterInstall(sLispFile, false);
end; { LispSetupBase }












{ /////////////////////////////////////////////////////////
//// Uninstallation }

{ Unfortunately this can not be used now }
var g_bToldNoEmacsDel : Boolean;
function TellNoEmacsDel(): Boolean;
begin
   Result := false;
   if g_bToldNoEmacsDel then exit;
   MsgBox('The Core Emacs files and downloaded lisp files will not be deleted.'
          +' This is because the installation process could not safely decide which '
          +'files where installed.',
          mbInformation, MB_OK);
   g_bToldNoEmacsDel := True;
end;







{ ///////////////////////////////////////////////////////// }
{//// Dialog pages creation }
{ ///////////////////////////////////////////////////////// }


procedure BrowseButtonOnClick(Sender: TObject); forward;
procedure TestButtonOnClick(Sender: TObject); forward;
procedure URLLabelOnClick(Sender: TObject); forward;
procedure URL2LabelOnClick(Sender: TObject); forward;

{ Are classes implemented?? Syntax?? type CFType = class; }





{ ///////////////////////////////////////////////////////////// }
{ //// FtypePage //// }


function FindAssoc(const sExt: string):string;
var sRes : string;
begin
   //Root: HKLM; Subkey: ".el"; ValueType: string; ValueName: ""; ValueData: "EmacsFile"
   { if RegQueryStringValue(HKLM, sExt, '', sRes) then begin }
   if ULRegQueryStringValue('SOFTWARE\Classes\'+sExt, '', sRes) then begin
      Result := sRes;
   end else begin
      Result := '';
   end;
end; { FindAssoc }

function SetAssoc(const sExt : string; const sFType : string):boolean;
begin
   { MsgBox('SetAssoc '+sExt+'=>'+sFType, mbinformation, mb_ok); }
   if StrGet(sExt, 1) <> '.' then begin
      InternalError('FAILED SetAssoc, first char is not "."');
   end;
   { Result := RegWriteStringValue(HKLM, sExt, '', sFType); }
   Result := ULRegWriteStringValue('SOFTWARE\Classes\'+sExt, '', sFType);
end; { SetAssoc }

function FindFTypeVerbs(sFtype : string): TArrayOfString;
var
   tasVerbs         : TArrayOfString;
begin
   { if RegGetSubkeyNames(HKLM, sFtype+'\shell', tasVerbs) then begin }
   if ULRegGetSubkeyNames(sFtype+'\shell', tasVerbs) then begin
      //MsgBox('got tasHKLM ' +inttostr(GetArrayLength(tasVerbs)) , mbinformation, mb_ok);
      Result := tasVerbs;
   end else begin
      { This will fail if the entry is not find in the registry but we do not have to tell }
      //InternalError('FAILED FindFtypeVerbs for sFType="'+sFtype+'"');
      SetArrayLength(tasVerbs, 0);
      Result := tasVerbs;
   end;
end; { FindFTypeVerbs }

function FindFtypeAction(sFtype  : string; sVerb : string): string;
var sRes : string;
begin
   Result := '';
   { if RegQueryStringValue(HKLM, sFtype+'\shell\'+sVerb+'\command', '', sRes) then begin }
   if ULRegQueryStringValue(sFtype+'\shell\'+sVerb+'\command', '', sRes) then begin
      Result := sRes;
   end;
end; { FindFtypeAction }

function SetFtypeAction(sFtype : string; sVerb : string; sAction : string): boolean;
begin
   { Result := RegWriteStringValue(HKLM, sFtype+'\shell\'+sVerb+'\command', '', sAction); }
   Result := ULRegWriteStringValue(sFtype+'\shell\'+sVerb+'\command', '', sAction);
end; { SetFtypeAction }

function FindExtAndFtype(
                             const sExtList : string;
                                            { users starting list of extensions }
                         var tstrlistFtype  : TStringList;
                                            { returns sorted list of found file types }
                         var atasVerbs      : array of TArrayOfString;
                                            { corresponding verbs }
                         var atasActs       : array of TArrayOfString;
                                            { corresponding verb actions }
                         var atstrlistExts  : array of TStringList
                                            { corresponding file extensions + free ext last }
                             )              : boolean;
var
   tasVerbs          : TArrayOfString;
   ntasVerbs         : integer;
   nn                : integer;
   n2                : integer;
   bDump             : boolean;
   tasHKCR           : TArrayOfString;
   ntasHKCR          : integer;
   sRes              : string;
   sExt              : string;
   tstrlistExtStart  : TStringList;
   sThisExt          : string;
   sThisAss          : string;
   nFtype            : integer;
   sFtype            : string;
   atstrlistExtsFree : TstringList;
   sVerb             : string;
   iRoot             : integer;
begin
   Result := false;

   tstrlistExtStart := TStringList.Create();
   tstrlistFtype    := TStringList.Create();
   tstrlistExtStart.CommaText := sExtList;

   atstrlistExtsFree := TstringList.Create();

   bDump:=false;
   { Collect file types }
   for nn:=0 to tstrlistExtStart.Count-1 do begin
      sThisExt := tstrlistExtStart.Strings[nn];
      { There is a serious bug here, FindAss returned the last value I believe!!! }
      if bDump then begin
            bDump := msgbox(sThisExt,
                            mbinformation, mb_okcancel)=idok;
         end;
      sThisAss := FindAssoc(sThisExt);
      if sThisAss <> '' then begin
         // not tstrlistFtype.Find(sThisAss)
         if bDump then begin
            bDump := msgbox(inttostr(tstrlistFtype.IndexOf(sThisAss)),
                            mbinformation, mb_okcancel)=idok;
         end;
         if -1=tstrlistFtype.IndexOf(sThisAss) then
            tstrlistFtype.Add(sThisAss);
      end else begin
         //tstrlistExtFree.Add(sThisExt);
         //tstrlistExtFree.Add(sThisExt);
         //atstrlistExts[tstrlistFtype.Count].Add(sThisExt);
         atstrlistExtsFree.Add(sThisExt);
      end;
   end;
   //tstrlistFtype.Duplicates := false; // Does not work???

   msgboxdbg(
   //MsgBox2(
          'EXT '+IntToStr(tstrlistExtStart.Count)+': '+tstrlistExtStart.CommaText
          +#13#10#13#10
          'FTYPE '+IntToStr(tstrlistFtype.Count)+': '+tstrlistFtype.CommaText
          );

   { sort the file type list so that we are sure where things are }
   tstrlistFtype.Sorted := true;
   tstrlistFtype.Sort();

   SetArrayLength(atstrlistExts, 0);
   SetArrayLength(atstrlistExts, tstrlistFtype.Count + 1); { +1 for free extensions }
   //msgbox(inttostr(tstrlistExtStart.Count), mbinformation, mb_ok);
   for nn:=0 to GetArrayLength(atstrlistExts)-1 do begin
      atstrlistExts[nn] := TstringList.Create();
   end;
   atstrlistExts[GetArrayLength(atstrlistExts)-1].CommaText := atstrlistExtsFree.CommaText;

   { Get a list of all top level reg keys in HKCR, will include all known extensions. }
   { In this case we really want to use HKCR! }
   if RegGetSubkeyNames(HKCR, '', tasHKCR) then begin
   { if ULRegGetSubkeyNames('', tasHKCR) then begin }
      //MsgBox('got tasHKCR ' +inttostr(GetArrayLength(tasHKCR)) , mbinformation, mb_ok);
   end else begin
      InternalError('FAILED tasHKCR in FindExtAndFtypes');
      exit;
   end;
   ntasHKCR := GetArrayLength(tasHKCR);
   bDump:=false;
   // Skip first two, they are not extensions
   // Check upto first not beginning whit "."
   for nn:=2 to ntasHKCR-1 do begin
      sExt := tasHKCR[nn];
      if false and bDump then begin
         bDump := msgbox(
                         'tasHKCR['+inttostr(nn)+']=('+sExt+')'
                         +' StrGet1='+StrGet(sExt, 1)
                         , mbinformation, mb_okcancel) = IDOK;
      end;
      if StrGet(sExt, 1) <> '.' then begin
         //msgbox(inttostr(nn), mbinformation, mb_ok);
         { reg list of extensions stops here }
         nn:=ntasHKCR;
      end else begin
         { If used by one of our file types add it }
         { if not RegQueryStringValue(HKLM, sExt, '', sRes) then begin }
         if not ULRegQueryStringValue('SOFTWARE\Classes\'+sExt, '', sRes) then begin
            { if RegValueExists(HKLM, sExt, '') then begin }
            if ULRegValueExists('SOFTWARE\Classes\'+sExt, '', iRoot) then begin
               InternalError('failed rgsv sext='+sExt);
               exit;
            end;
         end;
         if bDump then begin
            bDump := msgbox(
                            'tasHKCR['+inttostr(nn)+']=('+tasHKCR[nn]+')'
                            +', ('+sRes+')'
                            , mbinformation, mb_okcancel) = IDOK;
         end;
         nFtype := tstrlistFtype.IndexOf(sRes);
         if nFtype <> -1 then begin
            if -1=atstrlistExts[nFtype].IndexOf(sExt) then begin
               atstrlistExts[nFtype].Add(sExt);
            end;
         end;
      end;
   end;
   //msgbox('done tasHKCR', mbinformation, mb_ok);
   SetArrayLength(atasVerbs, 0);
   SetArrayLength(atasVerbs, tstrlistFtype.Count);
   SetArrayLength(atasActs, 0);
   SetArrayLength(atasActs, tstrlistFtype.Count);
   for nn:=0 to tstrlistFtype.Count-1 do begin
      sFtype := tstrlistFtype.Strings[nn];
      tasVerbs := FindFTypeVerbs(sFtype);
      atasVerbs[nn] := tasVerbs;
      ntasVerbs := GetArrayLength(tasVerbs);
      SetArrayLength(atasActs[nn], 0);
      SetArrayLength(atasActs[nn], ntasVerbs);
      for n2:=0 to ntasVerbs-1 do begin
         sVerb := tasVerbs[n2];
         atasActs[nn][n2] := FindFtypeAction(sFtype,sVerb);
      end;
   end;
   //msgbox('End Findextandftype', mbinformation, mb_ok);
   Result := true;
end; { FindExtAndFtype }



{ Glob vars for file type associations }
   { g_aAssChk  : array of TArrayOfString; }
        {
        g_aAssChk ...  0  = '' or 'checked'
                  ...  1  = name (.ext or file type name)
                  ...  2  = nonexistent (for .ext) or verb (for file type name)
        }
var
   g_bAssChk  : array of boolean;
   g_sEditChk : array of string;
   g_nAssChk  : integer;
   g_editChk  : array of TEdit;
   g_beditChk : array of boolean;
   g_tScroll  : TScrollingWinControl;
var
   g_tstrlistFtype  : TStringList;
   g_atasVerbs    : array of TArrayOfString;
   g_atasActs     : array of TArrayOfString;
   g_atstrlistExt         : array of TStringList;
   g_aAssPtr  : array of array of integer;
              { array of 0 - tstrlistFtype idx
                       1 - verb
                       2 - ext }

procedure AddAssIdxFtypeVerb(const idx      : integer;
                             const idxFtype : integer;
                             const idxVerb  : integer);
begin
   g_aAssPtr[idx][0] := idxFtype;
   g_aAssPtr[idx][1] := idxVerb;
   g_aAssPtr[idx][2] := -1;
end; { AddAssIdxVerbFtype }

procedure AddAssIdxFtypeExt(const idx      : integer;
                            const idxFtype : integer;
                            const idxExt   : integer);
begin
   g_aAssPtr[idx][0] := idxFtype;
   g_aAssPtr[idx][1] := -1
   g_aAssPtr[idx][2] := idxExt;
end; { AddAssIdxFtypeExt }

procedure AddAssIdxFreeExt(const idx         : integer;
                           const idxFreeVerb : integer);
begin
   g_aAssPtr[idx][0] := -1;
   g_aAssPtr[idx][1] := -1
   g_aAssPtr[idx][2] := idxFreeVerb;
end; { AddAssIdxFreeExt }


function AssIdxIsFreeExt(const idx  : integer):boolean;
begin
   Result := (g_aAssPtr[idx][0] = -1) and (g_aAssPtr[idx][1] = -1)
end; { AssIdxIsFreeExt }

function AssIdxToFtypeNum(const idx     : integer): integer;
begin
   Result := g_aAssPtr[idx][0];
end; { AssIdxToFtypeNum }

function AssIdxToFtype(const idx     : integer): string;
begin
   Result := g_tstrlistFtype[AssIdxToFtypeNum(idx)];
end; { AssIdxToFtype }

function AssIdxIsVerb(const idx : integer):boolean;
begin
   if (g_aAssPtr[idx][1] = -1) and (g_aAssPtr[idx][2] = -1) then begin
      InternalError('AssIdxIsVerb: neither verb or ext: '
                    +inttostr(idx)+' '+assIdxToFtype(idx));
   end;
   Result := g_aAssPtr[idx][1] <> -1;
end; { AssIdxIsVerb }

function AssIdxToVerb(const idx : integer):string;
begin
   Result := g_atasVerbs[AssIdxToFtypeNum(idx)][g_aAssPtr[idx][1]];
end; { AssIdxToVerb }

function AssIdxToAct(const idx : integer):string;
begin
   Result := g_atasActs[AssIdxToFtypeNum(idx)][g_aAssPtr[idx][1]];
end; { AssIdxToAct }

function AssIdxToNewActRaw(const idx :  integer): string;
begin
   //Result := Trim(g_editChk[idx].text);
   Result := g_sEditChk[idx];
end; { AssIdxToNewActRaw }

function AssIdxToNewAct(const idx :  integer): string;
var
   sNewAct :  string;
begin
   sNewAct := AssIdxToNewActRaw(idx);
   StringChange(sNewAct, '{EmacsRoot}', RemoveBackSlash(ExtractFileDir(emacs_dir_ToUse())));
   Result := sNewAct;
end; { AssIdxToNewAct }


function AssIdxToExt(const idx : integer):string;
var
   iFtypeNum :  integer;
begin
   iFtypeNum := AssIdxToFtypeNum(idx);
   if iFtypeNum = -1 then iFtypeNum := GetArrayLength(g_atstrlistExt)-1;
   Result := g_atstrlistExt[iFtypeNum][g_aAssPtr[idx][2]];
end; { AssIdxToExt }



procedure AssChkOnClick(Sender: TObject);
var
   chkSender : TCheckBox;
   nTag      : integer;
begin
   chkSender := TCheckBox(Sender);
   nTag := chkSender.tag;
   if chkSender.checked then begin
      //sChk := 'checked';
      g_bAssChk[nTag] := true;
      { if GetArrayLength(g_aAssChk[nTag]) = 3 then begin }
      if AssIdxIsVerb(ntag) then begin
         if not g_beditChk[nTag] then begin
            { seems like this will be put above in z-order! }
            g_beditChk[nTag] := true;
            g_editChk[nTag] := TEdit.Create(WizardForm);
            g_editChk[nTag].parent := g_tScroll;
            g_editChk[nTag].top := chkSender.top;
            g_editChk[nTag].left := chkSender.left + 16;
            { g_editChk[nTag].width := WizardForm.ScriptDlgPanel.Width - 50 - g_editChk[nTag].left; }
            g_editChk[nTag].width := WizardForm.Width - 50 - g_editChk[nTag].left;
            g_editChk[nTag].text := '"{EmacsRoot}\Emacs\bin\emacsclientw.exe" -n "%1"';
            g_editChk[nTag].taborder := chkSender.taborder + 1;
         end else begin
            g_editChk[nTag].visible := true;
         end;
      end;
   end else begin
      //sChk := '';
      g_bAssChk[nTag] := false;
      { if GetArrayLength(g_aAssChk[nTag]) = 3 then }
      if AssIdxIsVerb(ntag) then
         g_editChk[nTag].visible := false;
   end;
   { g_aAssChk[nTag][0] := sChk; }
end; { AssChkOnClick }

function AddFtypeChk(
                         AOwner : TWinControl;
                         sCap   : string;
                         bFtype : boolean;
                         sName  : string;
                         sVerb  : string;
                     var nTop   : integer;
                     var nCol   : integer
                     )          : TCheckBox;
var
   chk : TCheckBox;
   txt : TNewStaticText;
begin
   //if GetArrayLength(g_aAssChk) < g_nAssChk+5 then SetArrayLength(g_aAsschk, g_nAssChk+50);
   chk := TCheckBox.Create(AOwner);
   Result := chk;
   chk.Parent := g_tScroll;
   chk.Tag := g_nAssChk;
   chk.OnClick := @AssChkOnClick;

   if bFtype then begin
      myassert(sverb <> '', 'Ftype='+sName+', sVerb=""');
      txt := TNewStaticText.Create(AOwner);
      txt.Parent := g_tScroll;
      txt.TabStop := false;
      txt.wordwrap := true;
      txt.left := 24;
      txt.Width := AOwner.Width - 50 - txt.left;
      txt.Top := nTop;
      txt.autosize := true;
      txt.Caption := sCap;
      //txt.Color := clWhite;
      nTop := nTop + txt.height;

      chk.Top := nTop;
      //if Pos('\gnuclientw.exe', sCap) > 0 then begin
      if Pos('\emacsclient.exe', sCap) > 0 then begin
         chk.Caption := 'Check to change';
      end else begin
         chk.Caption := 'Check to use Emacs for this verb';
      end;
      chk.Left := 24 + 12;
      chk.Width := chk.Parent.Width - 50 - chk.left;
      nTop := nTop + chk.height + 12;

      { SetArrayLength(g_aAssChk[g_nAssChk], 0); }
      { SetArrayLength(g_aAssChk[g_nAssChk], 3); }
      SetArrayLength(g_aAssPtr[g_nAssChk], 0);
      SetArrayLength(g_aAssPtr[g_nAssChk], 3);
      { g_aAssChk[g_nAssChk][2] := sVerb; }
   end else begin
      myassert(sverb = '', 'dotext='+sName+', sVerb="'+sVerb+'"');
      chk.Top := nTop;
      if false then begin
         txt := TNewStaticText.Create(AOwner);
         txt.Parent := g_tScroll;
         txt.TabStop := false;
         txt.Width := 48;
         txt.autosize := false;
         txt.Top := nTop;
         txt.Caption := sCap;
         txt.Left := 24;
         chk.Caption := 'Check to use EmacsFile for this extension';
         //txt.Color := clWhite;
         chk.Left := txt.left + txt.width;
         chk.Width := chk.Parent.Width - 50 - chk.left;
         nTop := nTop + chk.Height + 0;
      end;
      chk.Caption := sCap;
      chk.Left := 24 + nCol * 67;
      nCol := nCol + 1;
      if nCol > 4 then begin
         nCol := 0;
         nTop := nTop + chk.Height + 0;
      end;
      { SetArrayLength(g_aAssChk[g_nAssChk], 0); }
      { SetArrayLength(g_aAssChk[g_nAssChk], 3); }
      SetArrayLength(g_aAssPtr[g_nAssChk], 0);
      SetArrayLength(g_aAssPtr[g_nAssChk], 3);
   end;
   //chk.OnClick := @AssChkOnClick;
   { g_aAssChk[g_nAssChk][1] := sName; }
   g_nAssChk := g_nAssChk + 1;
end; { AddFtypeChk }

var
   g_teditExts  : TEdit;
   g_lblNew2    : TNewStaticText;


procedure FTypeSkipOnClick(Sender: TObject);
var
   rad : TRadioButton;
begin
   rad := TRadioButton(Sender);
   if rad.Tag = 0 then begin
      g_teditExts.visible := false;
      g_lblNew2.visible := false;
      g_bSkipAssoc := true;
   end else begin
      g_teditExts.visible := true;
      g_lblNew2.visible := true;
      g_bSkipAssoc := false;
   end;
end; { FTypeSkipOnClick }

function MakeCommaText(const sIn : string; var sOut : string): boolean;
var
   ss : string;
begin
   Result := False;
   { just try to make a comma separated list of everyting the user may have typed }
   ss := sIn;
   StringChange(ss, #9,  ' ');  { make everyting that could be a divider a comma }
   StringChange(ss, #13, ' ');
   StringChange(ss, #10, ' ');
   StringChange(ss, ',', ' ');
   StringChange(ss, ';', ' ');
   StringChange(ss, ':', ' ');
   StringChange(ss, '  ', ' '); { remove space doublettes }
   StringChange(ss, '  ', ' '); { maybe need one more since doubled }
   ss := Trim(ss);
   StringChange(ss, ' ', ',');  { back to commas after trim }
   StringChange(ss, ',', ',.'); { add dots if missing }
   StringChange(ss, '..', '.');
   if Length(ss) = 0 then exit;
   if msgbox('ss='+ss, mbConfirmation, MB_OKCANCEL) <> IDOK then exit;
   if msgbox('ss1='+StrGet(ss,1), mbConfirmation, MB_OKCANCEL) <> IDOK then exit;
   if StrGet(ss, 1) <> '.' then msgbox('here', mbConfirmation, MB_OKCANCEL);
   if StrGet(ss, 1) <> '.' then ss:= '.'+ss;
   if msgbox(sIn+#13#10+ss, mbConfirmation, MB_OKCANCEL) <> IDOK then exit;
   sOut := ss;
   Result := True;
end; { MakeCommaText }


var
   g_oAssForm       : TForm;
   g_oAssFormOk     : TButton;
   g_oAssFormCancel : TButton;
var
   g_sExtList   : string;

procedure ResizeAssForm(Sender: TObject);
begin
   if g_oAssFormOk <> nil then begin
      g_tScroll.Left := 20;
      g_tScroll.Height := g_oAssForm.ClientHeight - g_oAssFormOk.Height - 20;
      g_tScroll.Width := g_oAssForm.ClientWidth - g_tScroll.Left;
      g_oAssFormOk.Top := g_oAssForm.ClientHeight - g_oAssFormOk.Height - 8;
      g_oAssFormCancel.Top := g_oAssFormOk.Top;
      g_oAssFormCancel.Left := g_oAssForm.ClientWidth - g_oAssFormCancel.Width - 10;
      g_oAssFormOk.Left := g_oAssFormCancel.left - g_oAssFormOk.Width - 10;
   end;
end; { ResizeAssForm }







function BuildTScroll(const bUseAssForm : boolean ): TWinControl;
var
   oFirst       : TWinControl;
   AOwner       : TWinControl;
   nTop         : integer;
   LblHdr       : TNewStaticText;
   LblNew       : TNewStaticText;
   nOfChk       : integer;
   ss           : string;
   nn           : integer;
   n2           : integer;
   lblFtypes    : array of TNewStaticText;
   lblExts      : array of TNewStaticText;
   chkFtypesAct : array of TCheckBox;
   nChk         : integer;
   nCol         : integer;
   sCap         : string;
   sExt         : string;
   sAction      : string;
   sVerb        : string;
   sFtype       : string;
begin
   if bUseAssForm then begin
      AOwner := g_oAssForm;
   end else begin
      { AOwner := WizardForm.ScriptDlgPanel; }
      { ScriptDlgPageSetSubCaption1('What file associations do you want to change?'); }
      RaiseException('AOwner := CreateCustomPage(''What file associations do you want to change?''); }');
   end;

   g_tScroll := TScrollingWinControl.Create(AOwner);
   g_tScroll.Parent := AOwner;
   //g_tScroll.HorzScrollBar := true;
   //g_tScroll.Color := clYellow;
   //g_tScroll.backcolor := clRed;
   g_tScroll.top := nTop;
   if not bUseAssForm then begin
      g_tScroll.Width := AOwner.Width;
      g_tScroll.Height := AOwner.Height - nTop - 4;
   end else begin
      ResizeAssForm(nil);
   end;

   if bUseAssForm then begin
      LblHdr := TNewStaticText.Create(g_oAssForm);
      LblHdr.Parent := g_tScroll;
      LblHdr.Font.Color := clMaroon;
      //lblHdr.Color := clWhite;
      LblHdr.Font.Size := 15;
      LblHdr.WordWrap := True;
      LblHdr.AutoSize := True;
      LblHdr.Left := 0;
      LblHdr.Top := 16;
      LblHdr.Width := g_oAssForm.ClientWidth - 2*32;
      LblHdr.Caption := 'Change File Associations to Emacs';
   end;


   LblNew := TNewStaticText.Create(AOwner);
   lblNew.Parent := g_tScroll;
   LblNew.WordWrap := True;
   LblNew.AutoSize := True;
   lblNew.Top := lblHdr.Top+lblHdr.Height+10;
   LblNew.Width := lblNew.Parent.Width - 50;
   LblNew.Caption := ExpandConstant('{cm:FileAssocChgHdr}');
   //lblNew.Color := clWhite;

   nTop := lblNew.Top + lblNew.Height + 16;
   //nTop := 0;

   nOfChk := 0;
   ss:='';
   for nn:= 0 to g_tstrlistFtype.Count-1 do begin
      ss := ss+#13#10#13#10+g_tstrlistFtype.Strings[nn];
      ss := ss+': ';
      ss := ss+g_atstrlistExt[nn].CommaText;
      nOfChk := nOfChk+g_atstrlistExt[nn].Count;
      //ss := ss+#13#10+'open='+findFtypeAction(tstrlistFtype.Strings[nn],'open');
      //ss := ss+#13#10+'edit='+findFtypeAction(tstrlistFtype.Strings[nn],'edit');
      for n2:=0 to GetArrayLength(g_atasVerbs[nn])-1 do begin
         ss := ss+#13#10+g_atasVerbs[nn][n2]+'='+g_atasActs[nn][n2];
         nOfChk  := nOfChk+1;
      end;
   end;
   nOfChk := nOfChk+g_atstrlistExt[g_tstrlistFtype.Count].Count; { free exts }


   { SetArrayLength(g_aAssChk, 0); }
   SetArrayLength(g_bAssChk, 0);
   SetArrayLength(g_sEditChk, 0);
   SetArrayLength(g_aAssPtr, 0);
   { SetArrayLength(g_aAssChk, nOfChk); }
   SetArrayLength(g_bAssChk, nOfChk);
   SetArrayLength(g_sEditChk, nOfChk);
   SetArrayLength(g_aAssPtr, nOfChk);
   SetArrayLength(g_editChk, 0); { more easy to do so }
   SetArrayLength(g_editChk, nOfChk); { more easy to do so }
   SetArrayLength(g_beditChk, 0); { more easy to do so }
   SetArrayLength(g_beditChk, nOfChk); { more easy to do so }
   g_nAssChk := 0;
   //msgbox(ss, mbinformation, mb_ok);

   SetArrayLength(lblFtypes, 0);
   SetArrayLength(lblFtypes, g_tstrlistFtype.Count);
   SetArrayLength(lblExts,   0);
   SetArrayLength(lblExts,   g_tstrlistFtype.Count+1); { +1 for not assoc exts }
   SetArrayLength(chkFtypesAct, 0);
   SetArrayLength(chkFtypesAct, nOfChk);

   nChk := 0;

   for nn:= 0 to g_tstrlistFtype.Count-1 do begin
      sFtype := g_tstrlistFtype.Strings[nn];
      if sFtype = 'EmacsFile' then begin
         lblFtypes[nn] := TNewStaticText.Create(AOwner);
         lblFtypes[nn].Parent := g_tScroll;
         lblFtypes[nn].TabStop := false;
         lblFtypes[nn].Top := nTop;
         lblFtypes[nn].WordWrap := true;
         lblFtypes[nn].Width := lblFtypes[nn].Parent.Width - 50;
         lblFtypes[nn].Caption := 'The extensions below are already associated with Emacs:';
         lblFtypes[nn].Font.Color := clMaroon;
         nTop := nTop + lblFtypes[nn].height + 8;
         nCol := 0;
         for n2 := 0 to g_atstrlistExt[nn].Count-1 do begin
            sExt := g_atstrlistExt[nn].Strings[n2];
            lblNew := TNewStaticText.Create(AOwner);
            lblNew.Parent := g_tScroll;
            lblNew.Caption := sExt;
            lblNew.Top := nTop;
            lblNew.Left := 24 + nCol * 67;
            nCol := nCol + 1;
            if nCol > 4 then begin
               nCol := 0;
               nTop := nTop + lblNew.Height + 0;
            end;
         end;
         if nCol <> 0 then nTop := nTop + lblNew.Height + 0;
         nTop := nTop + 20;
      end;
   end;

   nn := g_tstrlistFtype.Count;
   if g_atstrlistExt[nn].Count > 0 then begin
      lblExts[nn] := TNewStaticText.Create(AOwner);
      lblExts[nn].Parent := g_tScroll;
      lblExts[nn].TabStop := false;
      lblExts[nn].Top := nTop;
      lblExts[nn].WordWrap := true;
      lblExts[nn].Width := lblExts[nn].Parent.Width - 50 - lblExts[nn].left;
      lblExts[nn].Caption := ''
      +'The extensions below of those you gave '
      +'are not curently associated with any file type. '
      +'If you check an extension below it will be associated with EmacsFile.';
      lblExts[nn].Font.Pitch := 20;
      lblExts[nn].Font.Color := clMaroon;
      //lblExts[nn].Color := clWhite;
      //lblExts[nn].Font.style := 3;

      nTop := nTop + lblExts[nn].height + 8;
      nCol := 0;

      for n2 := 0 to g_atstrlistExt[nn].Count-1 do begin
         sExt := g_atstrlistExt[nn].Strings[n2];
         sCap := sExt;
         chkFTypesAct[nChk] := AddFtypeChk(AOwner, sCap, false, sExt, '', nTop, nCol);
         AddAssIdxFreeExt(nChk, n2);
         nChk := nChk+1;
      end;
      if nCol <> 0 then nTop := nTop + chkFTypesAct[0].Height + 0;
      nTop := nTop + 20;
   end;

   for nn:= 0 to g_tstrlistFtype.Count-1 do begin
      sFtype := g_tstrlistFtype.Strings[nn];
      if sFtype <> 'EmacsFile' then begin
         lblFtypes[nn] := TNewStaticText.Create(AOwner);
         lblFtypes[nn].Parent := g_tScroll;
         lblFtypes[nn].TabStop := false;
         lblFtypes[nn].Top := nTop;
         lblFtypes[nn].WordWrap := true;
         lblFtypes[nn].Width := lblFtypes[nn].Parent.Width - 50;
         lblFtypes[nn].Caption := 'Below is '+sFtype
         + ' file type. First comes a list of verbs (if any) and then '
         +'file extensions associated with this file type. '
         +'If you check a verb then '
         +'this file types name and icon are still used, '
         + 'but the emacsclient.exe will be used for performing the action.';
         lblFtypes[nn].Font.Pitch := 30;
         lblFtypes[nn].Font.Color := clMaroon;
         //lblFtypes[nn].Color := clWhite;
         //lblFtypes[nn].Font.style := 3;
         nTop := nTop + lblFtypes[nn].height + 8;

         //SetArrayLength(chkFtypesAct[nn], 0); {GetArrayLength(atasActs[nn]));}
         //SetArrayLength(chkFtypesAct[nn], nOfChk); {GetArrayLength(atasActs[nn]));}
         for n2 := 0 to GetArrayLength(g_atasActs[nn])-1 do begin
            sVerb := g_atasVerbs[nn][n2];
            sAction := g_atasActs[nn][n2];
            //if Pos('\gnuclientw.exe', sAction) > 0 then begin
            if Pos('\emacsclient.exe', sAction) > 0 then begin
               sCap := 'Verb "'+sVerb+'" already uses Emacs and runs'+#13#10+'    '+sAction;
            end else begin
               sCap := 'Verb "'+sVerb+'" runs'+#13#10+'    '+sAction;
            end;
            chkFTypesAct[nChk]
            := AddFtypeChk(AOwner, sCap, true, sFtype, sVerb, nTop, nCol);
            AddAssIdxFtypeVerb(nChk, nn, n2);
            nChk := nChk+1;
         end;

         { I do not get focus to this?? ... mixed up... }
         //oFirst := chkFTypesAct[0][0];
         oFirst := chkFTypesAct[0];
         Result := oFirst;

         lblExts[nn] := TNewStaticText.Create(AOwner);
         lblExts[nn].Parent := g_tScroll;
         lblExts[nn].TabStop := false;
         lblExts[nn].Top := nTop;
         lblExts[nn].Left := 24;
         lblExts[nn].WordWrap := true;
         lblExts[nn].Width := lblExts[nn].Parent.Width - 50 - lblExts[nn].left;
         lblExts[nn].Caption := ''
         + 'If you check an extension below it will be removed from the file type '
         +sFtype + ' and associated with EmacsFile instead.';
         lblExts[nn].Font.Pitch := 20;
         lblExts[nn].Font.Color := clMaroon;
         //lblExts[nn].Color := clWhite;
         //lblExts[nn].Font.style := 3;

         nTop := nTop + lblExts[nn].height + 8;
         nCol := 0;
         for n2 := 0 to g_atstrlistExt[nn].Count-1 do begin
            sExt := g_atstrlistExt[nn].Strings[n2];
            sCap := sExt;
            chkFTypesAct[nChk] := AddFtypeChk(AOwner, sCap, false, sExt, '', nTop, nCol);
            AddAssIdxFtypeExt(nChk, nn, n2);
            nChk := nChk+1;
         end;
         if nCol <> 0 then nTop := nTop + chkFTypesAct[0].Height + 0;
         nTop := nTop + 20;
      end;
   end;
end; { BuildTScroll }



procedure OnBuildTScroll(Sender: TObject);
begin
   BuildTScroll(true);
end; { OnBuildTScroll }

procedure DoAssChg();
var
   nn      : integer;
   sFtype  : string;
   sVerb   : string;
   sAction : string;
   sExt    : string;
begin
   for nn := 0 to g_nAssChk - 1 do begin
      if g_bAssChk[nn] then begin
         if AssIdxIsFreeExt(nn) then begin
            sFtype := 'EmacsFile';
            sExt := AssIdxToExt(nn);
            if not SetAssoc(sExt, sFType) then begin
               InternalError('FAILED SetAssoc('+sExt+', '+sFType+') -- was free');
            end;
         end else if AssIdxIsVerb(nn) then begin
            sFtype := AssIdxToFtype(nn);
            sVerb := AssIdxToVerb(nn);
            sAction := AssIdxToNewAct(nn);
            if not SetFtypeAction(sFtype, sVerb, sAction) then begin
               InternalError('FAILED SetFTypeAction');
            end;
         end else begin
            sFtype := 'EmacsFile';
            sExt := AssIdxToExt(nn);
            if not SetAssoc(sExt, sFType) then begin
               InternalError('FAILED SetAssoc('+sExt+', '+sFType+') -- was NOT free');
            end;
         end;
      end;
   end;
end; { DoAssChg }

procedure DumpChk(bSet : boolean);
var
   nn      : integer;
   ss      : string;
begin
   for nn := 0 to g_nAssChk - 1 do begin
      if g_bAssChk[nn] then begin
         ss := ss + #13#10;
         if AssIdxIsFreeExt(nn) then begin
            ss := ss + '**** Free ext: ' + AssIdxToExt(nn);
         end else if AssIdxIsVerb(nn) then begin
            ss := ss + '**** Ftype: ' + AssIdxToFtype(nn);
            ss := ss + #13#10;
            ss := ss + 'Verb: ' + AssIdxToVerb(nn);
            ss := ss + #13#10;
            ss := ss + '  Act: ' + AssIdxToAct(nn);
            ss := ss + #13#10;
            ss := ss + '  New Act: ' + AssIdxToNewAct(nn);
         end else begin
            ss := ss + '**** Ftype: ' + AssIdxToFtype(nn);
            ss := ss + #13#10;
            ss := ss + 'Ext: ' + AssIdxToExt(nn);
         end;
      end;
   end;
   if MsgBox(ss, mbConfirmation, MB_OKCANCEL) = IDOK then begin
      DoAssChg();
   end;
end; { DumpChk }

{ Avoid errors, g_editChk is freed! }
procedure PickupAssocEdits();
var
   nn :  integer;
begin
   for nn := 0 to g_nAssChk - 1 do begin
      if g_bAssChk[nn] then
         if not AssIdxIsFreeExt(nn) then
            if AssIdxIsVerb(nn) then g_sEditChk[nn] := Trim(g_editChk[nn].text);
   end;
end; { PickupAssocEdits }

procedure OnAssOk(Sender: TObject);
begin
   PickupAssocEdits();
   //DumpChk(true);
end; { OnAssOk }



function CreateAssForm(): TForm;
var
   Form        : TForm;
   //Button      : TButton;
begin
   Form := TForm.Create(Form);
   Form.Width := 500;
   Form.Height := 580;
   //Form.Color := clWhite;
   Form.Caption := 'EmacsW32 Setup - File Associations';
   Form.Position := poScreenCenter;
   Form.AutoScroll := false;
   //Form.OnCreate := @OnBuildTScroll;
   Form.OnShow := @OnBuildTScroll;

   g_oAssFormOk := TButton.Create(Form);
   g_oAssFormOk.Parent := Form;
   g_oAssFormOk.Top := Form.ClientHeight - g_oAssFormOk.Height - 10;
   g_oAssFormOk.Caption := 'OK';
   g_oAssFormOk.Cancel := False;
   g_oAssFormOk.ModalResult := mrOk;
   g_oAssFormOk.OnClick := @OnAssOk;

   g_oAssFormCancel := TButton.Create(Form);
   g_oAssFormCancel.Parent := Form;
   g_oAssFormCancel.Left := Form.ClientWidth - g_oAssFormCancel.Width - 10;
   g_oAssFormCancel.Top := Form.ClientHeight - g_oAssFormCancel.Height - 10;
   g_oAssFormCancel.Caption := 'Cancel';
   g_oAssFormCancel.Cancel := True;
   g_oAssFormCancel.ModalResult := mrCancel;
   g_oAssFormOk.Left := g_oAssFormCancel.left - g_oAssFormOk.Width - 10;

   g_oAssForm := Form;
   Form.OnResize := @ResizeAssForm;
   Form.ActiveControl := g_oAssFormOk;

   Result := Form;
end; { CreateAssForm }

function ShowAssForm(): integer;
begin
   CreateAssForm();
   Result := g_oAssForm.ShowModal();
   g_oAssForm.Release();
end; { ShowAssForm }





function CreateFtypePage(AfterID : integer): integer;
var
   Page        : TWizardPage;
   LblNew      : TNewStaticText;
   LblNoPrv    : TNewStaticText;
   iSubPage    : integer;
   iPrevPage   : integer;
   bNext       : boolean;
   oFirst      : TWinControl;
   radSkip     : TRadioButton;
   radDoIt     : TRadioButton;
   bStayOnPage : boolean;
   { Use separat form }
   bUseAssForm  : boolean;
begin
   result := -1;
   bUseAssForm := true;

   Page := CreateCustomPage(AfterID, 'File type associations',
                            'What file extensions could be involved?');
   g_FTypePageID := Page.ID;
   result := g_FTypePageID;

   LblNew := TNewStaticText.Create(Page);
   lblNew.Parent := Page.Surface;
   LblNew.WordWrap := True;
   LblNew.AutoSize := True;
   LblNew.Width := Page.SurfaceWidth;
   LblNew.Caption := ''
   +'Warning: File associations can be a bit dangerous to do. '
   +'If you do not understand what is done here it is better '
   +'that you skip this section! '
   +'You will however be able to choose what to do.'
   ;

   g_bSkipAssoc := true;

   radSkip := TRadioButton.Create(Page);
   radSkip.parent := Page.Surface;
   radSkip.top := lblNew.top + lblNew.height+12;
   radSkip.width := Page.SurfaceWidth;
   radSkip.Caption := 'Thank you, I will skip this section! But please associate .el with Emacs.';
   radSkip.font.color := $000099; //clMaroon;
   radSkip.checked := g_bSkipAssoc;
   radSkip.tag := 0;

   if false and not ForAllUsers() then begin
      lblNoPrv := TNewStaticText.Create(Page);
      lblNoPrv.Parent := Page.Surface;
      lblNoPrv.top := radskip.top + radskip.height + 40;
      lblNoPrv.WordWrap := True;
      lblNoPrv.AutoSize := True;
      lblNoPrv.Width := Page.SurfaceWidth;
      lblNoPrv.font.color := clRed;
      lblNoPrv.Caption := ''
      +'Sorry, you do not have privileges to change file associations.';
   end else begin
      radSkip.OnClick := @FTypeSkipOnClick;

      radDoit := TRadioButton.Create(Page);
      radDoit.parent := Page.Surface;
      radDoit.top := radSkip.top + radSkip.height + 0;
      radDoIt.width := Page.SurfaceWidth;
      radDoit.Caption := 'Thank you for your kindness but I know what I am doing!';
      raddoit.font.color := $004600; //clOlive;
      radDoIt.Checked := not g_bSkipAssoc; { radSkip.Checked; }
      radDoIt.tag := 1;
      radDoIt.OnClick := @FTypeSkipOnClick;

      g_LblNew2 := TNewStaticText.Create(Page);
      g_lblNew2.Parent := Page.Surface;
      g_lblNew2.top := radDoit.top + radDoIt.height + 12;
      g_LblNew2.WordWrap := True;
      g_LblNew2.AutoSize := True;
      g_LblNew2.Width := Page.SurfaceWidth;
      g_lblNew2.visible := false;
      g_LblNew2.Caption := ''
      +'OK. A short explanation. File associations are done in two steps. '
      +'First a "file type" is set up. '
      +'It include what to do in different cases. '
      +'It may also include an icon to show for associated file extensions. '
      +'Second, files with certain extensions are associated with the file type. '
      +'This means that changes could be made on two levels, '
      +'to the file type or in the association. '
      +'You will get a chance to alter both.'
      +#13#10
      +#13#10
      +'First please give a list of file extensions that you think '
      +'you might want to change:';

      if g_sExtList = '' then
         g_sExtList := ''
         +'.txt,.org,.c,.h,.cc,.cpp,.hpp,.cxx,.hxx,.pas,.pl,.pm,.bas,.cls'
         +',.dcl,.idl,.pro,.java,.m4,.mak,.tex,.xml,.sgml,.sql';
      g_teditExts := TEdit.Create(Page);
      g_teditExts.Parent := Page.Surface;
      g_teditExts.Top := g_lblNew2.Top + g_lblNew2.Height + 4;
      g_teditExts.Text := g_sExtList;
      g_teditExts.Width := Page.SurfaceWidth;
      g_teditExts.visible := false;
   end;
   oFirst := radSkip;
   {
   ScriptDlgPageClearCustom();
   oFirst := BuildTScroll(bUseAssForm);

   bNext := ScriptDlgPageProcessCustom(oFirst);
   if Terminated() then exit;
   }


   bStayOnPage := false;
   if iSubPage = 0 then begin
   end else begin
      g_tScroll.Free();
   end;
   iPrevPage := iSubPage;
   if not bStayOnPage then begin
      if bNext then begin
         isubpage := isubpage+1
         if radSkip.checked then isubpage := isubpage + 9;
      end else isubpage := isubpage-1;
   end;
//end;

//PickupAssocEdits();
//Result := bNext;

//ScriptDlgPageClose(not bNext);
//ScriptDlgPageClose(true);

end; { CreateFtypePage }

function NextOnFtypePage(): boolean;
var
   bStayOnPage : boolean;
begin
   if not g_bSkipAssoc then begin
      bStayOnPage := true;
      if MakeCommaText(g_teditExts.Text, g_sExtList) then begin
         if FindExtAndFtype(g_sExtList, g_tstrlistFtype,
                            g_atasVerbs, g_atasActs, g_atstrlistExt)
            then begin
               bStayOnPage := false;
               case ShowAssForm() of
                 mrOK : begin
                           g_bSkipAssoc := false;
                        end;
                 mrCancel : begin
                               g_bSkipAssoc := true;
                               bStayOnPage := true;
                            end;
               end;
            end;
      end else begin
         MsgBox('Please enter a list of file extensions (like ".htm, .txt")',
                mbInformation, MB_OK);
      end;
   end;
   result := not bStayOnPage;
end; { NextOnFtypePage }












{ ///////////////////////////////////////////////////////////// }
{ //// VanillaSubDirPage //// }

function FileNameSyntaxIsOk(sFileName   : string): boolean;
var
   sBadChar : string;
   iBC      : integer;
   sB       : string;
begin
   result := false;
   { Copy to local var to avoid error "Out of Global Vars range" }
   sB := g_csBadFileNameChars;
   for iBC := 1 to length(g_csBadFileNameChars) do begin
      sBadChar := StrGet(sB, iBC);
      if Pos(sBadChar, sFileName) > 0 then exit;
   end;
   result := true;
end; { FileNameSyntaxIsOk }

function ShouldSkip4VanillaSubDirPage(): boolean;
begin
   result := not DidSelUnpacking();
end; { ShouldSkip4VanillaSubDirPage }

function CreateVanillaSubDirPage(AfterID : integer): integer;
var
   Page           : TInputQueryWizardPage;
   sMsg           : string;
begin
   sMsg := 'Emacs itself will be installed in a subdirectory'
   +' of the directory you choosed for installation (' + g_sAPP +').'
   +' You can for example include version number in the name'
   +' of this subdirectory if you want to.';
   if EmacsIsInstalled() then
      sMsg := sMsg +#13#10 +#13#10
      +'Your current site-lisp directory will be copied to the new Emacs.' ;
   Page := CreateInputQueryPage(AfterID, 'Emacs Own Subdirectory',
                                'Subdirectory for Emacs itself', sMsg);
   g_VanillaPageID := Page.ID;
   result := g_VanillaPageID;
   Page.Add('Subdirectory for Emacs itself:', false);
end; { VanillaSubDirPage }

function NextOnVanillaSubDirPage(): boolean;
var
   Page           : TInputQueryWizardPage;
   sVanillaSubDir : string;
begin
   Page := TInputQueryWizardPage( PageFromID(g_VanillaPageID) );
   sVanillaSubDir := Page.Values[0];
   if FileNameSyntaxIsOk(sVanillaSubDir) then begin
      g_emacs_dir_ToInstall := AddBackSlash(g_sAPP) + sVanillaSubDir;
      result := true;
   end else begin
      MsgCriticalEx('BadSubDirName', g_csBadFileNameChars);
      result := false;
   end;
end; { NextOnVanillaSubDirPage }




function CreateSelEmacsPage(AfterID : integer): integer;
var
   Page : TInputDirWizardPage;
   sMsg : string;
begin
   sMsg := 'EmacsW32 Setup will install (or look for unless Basic Setup choosen) supporting files for Emacs'
   +' in the grand parent folder to the folder you select below.'
   +' The folder you select must be the folder where emacs.exe is.'
   +#13#10#13#10
   +'To continue, click Next. If you would like to select a different folder, click Browse.';
   Page := CreateInputDirPage(AfterID,
                              'Select the Folder Where Emacs is Located',
                              'Which Emacs of those you have should be used?',
                              sMsg,
                              false, '');
   g_SelEmacsPageID := Page.ID;
   result := g_SelEmacsPageID;
   Page.add('Folder where emacs.exe is:');
end; { CreateSelEmacsPage }

function ShouldSkip4SelEmacsPage(): boolean;
var
   Page      : TInputDirWizardPage;
   sEmacsDir : string;
begin
   { if g_bIncludesEmacs or DidSelUnpacking() then begin }
   if DidSelEmacsItself() or DidSelUnpacking() then begin
      result := true;
   end else begin
      Page := TInputDirWizardPage( PageFromID(g_SelEmacsPageID) );
      sEmacsDir := emacs_dir_ForSelect();
      //msgbox2('sEmacsDir 1='+sEmacsDir);
      if length(sEmacsDir) = 0 then begin
         result := false;
      end else begin
         sEmacsDir := AddBackSlash( emacs_dir_ForSelect() ) + 'bin';
         //msgbox2('sEmacsDir='+sEmacsDir);
         Page.values[0] := sEmacsDir;
         result := false;
      end;
   end;
end; { ShouldSkip4SelEmacsPage }

function AppDirHasEmacsW32Files(sApp : string): string;
var
   sFile : string;
begin
   result := '';
   //sFile := AddBackSlash(sApp)+'EmacsW32\bin\gnuclientw.exe';
   sFile := AddBackSlash(sApp)+'EmacsW32\lisp\emacsw32.el';
   if not FileExists(sFile) then
      result := result + 'Can''t find '+sFile+'.';
end; { AppDirHasEmacsW32Files }

function NextOnSelEmacsPage()        : boolean;
var
   Page         : TInputDirWizardPage;
   sEmacsBinDir : string;
   sEmacsExe    : string;
   sEmacsDir    : string;
   sErr         : string;
begin
   result := false;
   Page := TInputDirWizardPage( PageFromID(g_SelEmacsPageID) );
   sEmacsBinDir := Page.Values[0];
   g_emacs_dir_Selected := '';
   if length(sEmacsBinDir) > 0 then begin
      sEmacsExe := AddBackSlash(sEmacsBinDir) + 'emacs.exe';
      if FileExists(sEmacsExe) then begin
         g_emacs_dir_Selected := RemoveBackSlash(ExtractFileDir(sEmacsBinDir));
         if Check_emacs_dir_Ok() then begin
            sEmacsDir := ExtractFileDir(g_emacs_dir_Selected);
            sErr := AppDirHasEmacsW32Files(sEmacsDir);
            if ('' = sErr) or DidSelBasicSetup() then begin
               g_sApp := AddBackSlash(ExtractFileDir(ExtractFileDir(g_emacs_dir_Selected)));
               result := true;
            end else begin
               MsgBox2(sErr+
                       ' Please choose another directory or include "Basic Emacs Setup".');
            end;
         end else begin
            g_emacs_dir_Selected := '';
         end;
      end else begin
         MsgBox2('Can''t find "'+sEmacsExe+'".'+
                 ' Please choose the directory where emacs.exe is!');
      end;
   end else begin
      MsgBox2('Please choose a directory');
   end;
   //msgbox2('emacs_dir_Selected='+g_emacs_dir_Selected);
end; { NextOnSelEmacsPage }



function CreateAppDirPage(AfterID : integer): integer;
var
   Page : TInputDirWizardPage;
   sMsg : string;
   sApp : string;
begin
   sMsg := 'EmacsW32 Setup will install some supporting files in the folder below.'
   +' Emacs itself will be installed in a subdirectory which you choose later.'
   +#13#10#13#10
   +'To continue, click Next. If you would like to select a different folder, click Browse.';
   { sApp := g_sApp; }
   { sApp := ExtractFileDir(g_emacs_dir_FromReg); }
   //msgbox2('forsel='+emacs_dir_ForSelect());
   sApp := ExtractFileDir(emacs_dir_ForSelect());
   if length(sApp) = 0 then sApp := ExpandConstant('{pf}\Emacs');
   Page := CreateInputDirPage(AfterID,
                              'Select Destination Location',
                              'Where should EmacsW32 be installed?',
                              sMsg,
                              true, 'Emacs');
                              { false, sApp); }
   g_AppDirPageID := Page.ID;
   result := g_AppDirPageID;
   Page.add('Folder:');
   Page.values[0] := sApp;
end; { CreateAppDirPage }

function NextOnAppDirPage(): boolean;
var
   Page : TInputDirWizardPage;
   sApp : string;
begin
   Page := TInputDirWizardPage( PageFromID(g_AppDirPageID) );
   sApp := Page.Values[0];
   if length(sApp) > 0 then begin
      g_sApp := sApp;
      result := true;
   end else begin
      MsgBox2('Please choose a directory');
      result := false;
   end;
end; { NextOnAppDirPage }








{ ///////////////////////////////////////////////////////////// }
{ //// HomePage //// }

{ function CreateHomePage(AfterID : integer) : integer; }
{ var }
{    Page : TInputOptionWizardPage; }
{ begin    }
{    {\ }
{    ScriptDlgPageSetCaption(ExpandConstant('{\cm:CapHomePg')); }
{    ScriptDlgPageSetSubCaption1(ExpandConstant('{\cm:Sub1HomePg')); }
{    ScriptDlgPageSetSubCaption2(ExpandConstant('{\cm:Sub2HomePg')); }
{    Next := InputOption(ExpandConstant('{\cm:ChkDontSetHome'), }
{                        sDontSetHome); }
{    ScriptDlgPageClose(True); }
{    !{\ }
{    Page := CreateInputOptionPage(AfterID, }
{                                  ExpandConstant('{\cm:CapHomePg!{\'), }
{                                  ExpandConstant('{\cm:Sub1HomePg!{\'), }
{                                  ExpandConstant('{\cm:Sub2HomePg!{\'), }
{                                  false, false); }
{    g_HomePageID := Page.ID; }
{    result := g_HomePageID; }
{    Page.add( ExpandConstant('{\cm:ChkDontSetHome!{\') ); }
{    Page.Values[0] := false; }
{ end; {\ CreateHomePage !{\ }

{ function NextOnHomePage(): boolean; }
{ begin }
{    result := true; }
{ end; {\ NextOnHomePage !{\ }

{ function CreateSetHomePage(AfterID : integer) : integer; }
{ var }
{    Page      : TInputDirWizardPage; }
{    {\ sUserName : string; !{\ }
{    sHomeDir  : string; }
{ begin         }
{    {\ sUserName := GetUserNameString(); !{\ }
{    {\ StringChange(sUserName, ' ', ''); !{\ }
{    {\ sHomeDir := 'C:\Home\'+ sUserName; !{\ }
{    sHomeDir := GetShellFolder(false, sfDocs); }
{    {\ According to the discussions on Emacs Devel USERPROFILE will be the new default !{\ }
{    sHomeDir := GetEnv('USERPROFILE'); }
{    Page := CreateInputDirPage(AfterID, }
{                               ExpandConstant('{\cm:CapSetHomePg!{\'), }
{                               ExpandConstant('{\cm:Sub1SetHomePg!{\'), }
{                               ExpandConstant('{\cm:Sub2SetHomePg!{\'), }
{                               false, ''); }
{                               {\ true, sUserName); !{\ }
{    Page.add(''); }
{    Page.Values[0] := sHomeDir; }
{    g_SetHomePageID := Page.ID; }
{    result := g_SetHomePageID; }
{ end; {\ CreateSetHomePage !{\ }

{ function NextOnSetHomePage(): boolean; }
{ var }
{    Page     : TInputDirWizardPage; }
{    sHomeDir : string; }
{ begin }
{    Page := TInputDirWizardPage( PageFromID( g_SetHomePageID ) ); }
{    sHomeDir := Page.Values[0]; }
{    if IsCorrectDir(sHomeDir) then begin }
{       g_sNewHomeDir := sHomeDir; }
{       //MsgBox(sHomeDir, mbError, MB_OK); }
{       //g_bReboot := true; // not needed, change is broadcasted! }
{       result := true; }
{    end else begin }
{       //MsgBox(SetupMessage(msgInvalidPath), mbError, MB_OK); }
{       g_sNewHomeDir := ''; }
{       g_bReboot := false; }
{       result := false; }
{    end; }
{ end; {\ NextOnSetHomePage !{\ }





{ ///////////////////////////////////////////////////////////// }
{ //// TarGzPage //// }

function CreateTarGzPage(AfterID : integer): integer;
var
   Page           : TWizardPage;
   BrowseButton   : TButton;
   TestButton     : TButton;
   LblNew         : TNewStaticText;
   lblURLLabel    : TNewStaticText;
   URLLabel       : TNewStaticText;
   lblURLLabel2   : TNewStaticText;
   URLLabel2      : TNewStaticText;
   stxtEdTarGz    : TNewStaticText;
   sInstructions0 : String;
   //bFin                 : Boolean;
begin
   Page := CreateCustomPage(AfterID,
                            'Select Emacs Distribution File',
                            'Where is the Emacs Full Binary '
                            +'Distribution File for MS Windows?');
   g_TarGzPageID := Page.ID;
   result := g_TarGzPageID;
   sInstructions0 := ''
   +'This setup program assumes that you have downloaded the '
   +'Emacs binary distribution file for MS Windows separately. '
   +'If you have not already done so you can use the link below '
   +'to open the download page in your web browser . '
   +'The file you need is named something like "emacs-22.0.50-fullbin-i386.tar.gz". '
   +'I guess it will be about 20MB big, BUT THERE IS NO SUCH FILE TO DOWNLOAD YET WHEN I AM WRITING THIS!'
   +#13#10
   +#13#10
   +'** Please use the Emacs+EmacsW32 distribution instead! This includes Emacs itself. **'
   +#13#10
   +'For a link to the EmacsW32 site where you can find this look under Help below.'
   ;

   LblNew := TNewStaticText.Create(WizardForm);
   LblNew.Parent := Page.Surface;
   LblNew.Font.Color := clMaroon;
   LblNew.WordWrap := True;
   LblNew.AutoSize := True;
   LblNew.Width := Page.SurfaceWidth;
   LblNew.Caption := sInstructions0;

   lblURLLabel := TNewStaticText.Create(WizardForm);
   lblURLLabel.Parent := Page.Surface;
   lblURLLabel.Caption := 'Download here:';
   lblURLLabel.Top := LblNew.Top + LblNew.Height + 8;
   lblURLLabel.AutoSize := true;

   URLLabel := TNewStaticText.Create(WizardForm);
   URLLabel.Parent := Page.Surface;
   URLLabel.Top := lblURLLabel.Top;
   URLLabel.Left := lblURLLabel.width + 8;
   URLLabel.Caption := 'http://ftp.gnu.org/pub/gnu/emacs/windows/';
   URLLabel.Font.Style := URLLabel.Font.Style + [fsUnderLine];
   URLLabel.Font.Color := clBlue;
   URLLabel.Cursor := crHand;
   URLLabel.OnClick := @URLLabelOnClick;
   //No use for tabstop, it does not work to use keyboard here!
      //URLLabel.TabStop := True;
   lblURLLabel.FocusControl := URLLabel;

   lblURLLabel2 := TNewStaticText.Create(WizardForm);
   lblURLLabel2.Parent := Page.Surface;
   lblURLLabel2.Caption := 'Emacs home:';
   lblURLLabel2.Top := lblURLLabel.Top + lblURLLabel.Height + 2;
   lblURLLabel2.AutoSize := true;

   URLLabel2 := TNewStaticText.Create(WizardForm);
   URLLabel2.Parent := Page.Surface;
   URLLabel2.Top := lblURLLabel2.Top
   URLLabel2.Left := lblURLLabel.width + 8;
   URLLabel2.Caption := 'http://www.gnu.org/software/emacs/';
   URLLabel2.Font.Style := URLLabel.Font.Style + [fsUnderLine];
   URLLabel2.Font.Color := clBlue;
   URLLabel2.Cursor := crHand;
   URLLabel2.OnClick := @URLLabelOnClick;
   //No use for tabstop, it does not work to use keyboard here!
      //URLLabel.TabStop := True;
   lblURLLabel2.FocusControl := URLLabel2;

   stxtEdTarGz := TNewStaticText.Create(WizardForm);
   sTxtEdTarGz.Parent := Page.Surface;
   stxtEdTarGz.Top := URLLabel2.Top + URLLabel2.Height + 40;
   sTxtEdTarGz.Caption := 'GNU &Emacs binary distribution file for MS Windows:';
   sTxtEdTarGz.AutoSize := true;

   g_teditTarGz := TEdit.Create(Page.Surface);
   g_teditTarGz.Parent := Page.Surface;
   g_teditTarGz.Top := stxtEdTarGz.Top + stxtEdTarGz.Height + 4;
{    g_sEmacsTarGz := ReadRegEmacsTarGz(); }
{    g_teditTarGz.Text := g_sEmacsTarGz; }
   g_teditTarGz.Width := Page.SurfaceWidth - WizardForm.Cancelbutton.Width - 8;
   sTxtEdTarGz.FocusControl := g_teditTarGz;

   BrowseButton := TButton.Create(Page.Surface);
   BrowseButton.Parent := Page.Surface;
   BrowseButton.Top := g_teditTarGz.Top - 2;
   BrowseButton.Left := g_teditTarGz.Left + g_teditTarGz.Width + 8;
   BrowseButton.Caption := 'B&rowse';
   BrowseButton.OnClick := @BrowseButtonOnClick;

   TestButton := TButton.Create(Page.Surface);
   TestButton.Parent := Page.Surface;
   TestButton.Top := g_teditTarGz.Top + 20;
   TestButton.Left := g_teditTarGz.Left + g_teditTarGz.Width + 8;
   TestButton.Caption := '&Test';
   TestButton.OnClick := @TestButtonOnClick;
   TestButton.Visible := false;

end; { TarGzPage }

function ShouldSkip4TarGzPage(): boolean;
begin
   g_sEmacsTarGz := ReadRegEmacsTarGz();
   g_teditTarGz.Text := g_sEmacsTarGz;
   result := not DidSelUnpacking();
end; { ShouldSkip4TarGzPage }

function StrToIntProtected(str    : string;
                           defalt : integer): integer; forward;


function NextOnTarGzPage()     : boolean;
var
   sVanillaSubDir : string;
   sVer           : string;
   iVer           : integer;
   Page           : TInputQueryWizardPage;
   sSub           : string;
   iPos           : integer;
begin
   //msgbox2('nextOnTarGzPage');
   result := false;
   if length(g_teditTarGz.text) = 0 then begin
   end else if not CheckTarGz(g_sEmacsTarGz) then begin
   end else begin
      Page := TInputQueryWizardPage( PageFromID(g_VanillaPageID) );
      sSub := ExtractFileName(g_sEmacsTarGz);
      sSub := Copy(sSub, 1, length(sSub) - length(ExtractFileExt(sSub)));
      iPos := Pos('-fullbin', sSub);
      if iPos > 0 then sSub := Copy(sSub, 1, iPos-1);
      sVanillaSubDir := sSub;
      sVer := sSub;
      iPos := Pos('-', sVer);
      if iPos > 0 then begin
         sVer := Copy(sSub, iPos+1, Length(sSub) - iPos);
         //msgbox2(sVer);
         iPos := Pos('.', sVer);
         sVer := Copy(sVer, 1, iPos-1);
         //msgbox2(sVer);
         iVer := StrToIntProtected(sVer, 22);
         if iVer < 22 then begin
            if MsgQuery('Need22AskCont', MB_YESNO or MB_DEFBUTTON2) = IDNO then
               exit;
         end;
      end;
      Page.Values[0] := sVanillaSubDir;
      g_sEmacsTarGz := g_teditTarGz.Text;
      WriteRegEmacsTarGz;
      result := true;
   end;
end; { NextOnTarGzPage }





var CheckListBox        : TNewCheckListBox;
procedure CheckFtypeChecks(Sender :TObject );
begin
   msgbox(inttostr(Checklistbox.items.count), mbInformation, MB_ok);
end; { CheckFtypeChecks }





{ ///////////////////////////////////////////////////////////// }
{ //// ErrPage //// }

function CreateErrPage(AfterID : integer): integer;
var
   Page           : TWizardPage;
begin
   Page := CreateCustomPage(AfterID,
                            '*** ERROR ****',
                            'A fatal error occured. Intallation could not be finished.');
   g_ErrPageID := Page.ID;

   g_lblErrFin := TNewStaticText.Create(Page);
   g_lblErrFin.Parent := Page.Surface;
   g_lblErrFin.WordWrap := True;
   g_lblErrFin.AutoSize := True;
   g_lblErrFin.Width := Page.SurfaceWidth;
   g_lblErrFin.Caption := '';

   result := g_ErrPageID;
end; { ErrPage }

{ function ShouldSkip4ErrPage(): boolean; }
{ begin }
{    result := true; }
{    if (0 < Length(g_sErrFin)) then begin }
{       g_lblErrFin.Caption := g_sErrFin; }
{       result := false; }
{    end; }
{ end; {\ ShouldSkip4VanillaSubDirPage !{\ }
{ function ShouldSkip4FinPage(): boolean; }
{ begin }
{    result := (0 < Length(g_sErrFin)); }
{ end; {\ ShouldSkip4FinPage !{\ }


function NextOnErrPage(): boolean;
begin
   MsgCritical('ErrCantContinue');
   result := true;
end; { NextOnErrPage }








procedure ShowHelpForInstallation();
var
   iRes : integer;
   sUrl : string;
   sPar : string;
begin
   { ExtractTemporaryFile('etc\HelpEmacsW32InstWiz.html'); }
   ExtractTemporaryFile('iw-file-type-associations.png');
   ExtractTemporaryFile('iw-change-file-associations.png');
   ExtractTemporaryFile('iw-ready-to-install.png');
   ExtractTemporaryFile('EmacsW32.css');
   if g_bIncludesEmacs then begin
      sUrl := ExpandConstant('{tmp}\HelpEmacsEmacsW32InstWiz.html');
      ExtractTemporaryFile('HelpEmacsEmacsW32InstWiz.html');
      ExtractTemporaryFile('iw-select-tasks-emacs+ew32.png');
   end else begin
      sUrl := ExpandConstant('{tmp}\HelpEmacsW32InstWiz.html');
      ExtractTemporaryFile('HelpEmacsW32InstWiz.html');
      ExtractTemporaryFile('iw-select-tasks-ew32.png');
   end;

   sPar := '';
   case g_iCurPageID of
     wpSelectComponents : begin
        sPar := '#wpSelectComponents';
     end;
     g_AppDirPageID     : begin
     end;
     g_SelEmacsPageID   : begin
     end;
     g_TarGzPageID      : begin
     end;
     g_VanillaPageID    : begin
     end;
     g_FTypePageID      : begin
     end;
     g_ErrPageID        : begin
     end;
   end; { case }
   { This does not work:
   sUrl := sUrl+sPar; }
   if not ShellExec('open', sUrl, '', '', SW_SHOWNORMAL, ewNoWait, iRes) then
   begin
      InternalErrorWithCode(iRes,
                            '(when opening Installation Help file '
                            +sUrl+')');
   end;
end;


procedure HelpForInstallationOnClick(Sender: TObject);
begin
   ShowHelpForInstallation();
end; { HelpForInstallationOnClick }

procedure HelpForInstallationOnKeyPress(Sender: TObject);
begin
   ShowHelpForInstallation();
end; { HelpForInstallationOnClick }



procedure ShowHelpForm();
var
   Form         : TForm;
   LblHdr       : TNewStaticText;
   LblNew       : TNewStaticText;
   Button       : TButton;
   lblURL2Label : TNewStaticText;
   URL2Label    : TNewStaticText;
   lblURL3Label : TNewStaticText;
   URL3Label    : TNewStaticText;
   sGnu         : string;
   lblGnu       : TNewStaticText;
   sHasEmacs    : string;
begin
   Form := TForm.Create(Form);
   Form.Width := 420;
   Form.Height := 500;
   Form.Color := clWhite;
   if g_bIncludesEmacs then begin
      Form.Caption := 'Help - EmacsW32+Emacs Installer';
   end else begin
      Form.Caption := 'Help - EmacsW32 Setup Helper';
   end;
   Form.Position := poScreenCenter;
   Form.AutoScroll := true;

   sGnu := ''
+'  ,           , '    +#13#10
+' /             \ '    +#13#10
+'((__-^^-,-^^-__)) '    +#13#10
+' `-_---" `---_-" '    +#13#10
+'  `--|o` "o|--" '    +#13#10
+'     \  `  / '    +#13#10
+'      ): :( '    +#13#10
+'      :o_o: '    +#13#10
+'       "-" '
   ;

   LblHdr := TNewStaticText.Create(Form);
   LblHdr.Parent := Form;
   LblHdr.Font.Color := clMaroon;
   LblHdr.Font.Size := 15;
   LblHdr.WordWrap := True;
   LblHdr.AutoSize := True;
   LblHdr.Left := 32;
   LblHdr.Top := 32;
   LblHdr.Width := Form.ClientWidth - 2*32;
   LblHdr.Caption := 'EmacsW32 - Adjustments for'
   +#13#10
   +'Emacs MS Windows Users';

   lblGnu := TNewStaticText.Create(Form);
   lblGnu.Parent := Form;
   lblGnu.Font.Color := clYellow;
   lblGnu.Font.Color := $0ecdee;
   lblGnu.Font.Size := 4;
   lblGnu.Font.Pitch := 9;
   lblGnu.Font.Name := 'Courier New';
   lblGnu.WordWrap := False;
   lblGnu.AutoSize := True;
   lblGnu.Left := lblHdr.Width+lblHdr.left+5;
   lblGnu.Top := lblHdr.Top+10;
   //lblGnu.Width := Form.ClientWidth - 2*32;
   lblGnu.Caption := sGnu;

   LblNew := TNewStaticText.Create(Form);
   LblNew.Parent := Form;
   //LblNew.Font.Color := clMaroon;
   LblNew.WordWrap := True;
   LblNew.AutoSize := True;
   LblNew.Left := lblHdr.Left;
   LblNew.Top := LblHdr.Top + LblHdr.Height + 32;
   LblNew.Width := Form.ClientWidth - 2*32;
   if g_bIncludesEmacs then begin
      sHasEmacs :=
      '* The setup program You are running now is the bigger type that includes Emacs itself.'
      + #13#10
      + #13#10
      +'Beside installing Emacs itself the following can also be done by this installer:'
      ;
   end else begin
      sHasEmacs :=
      '* The setup program You are running now is the smaller type that does NOT include Emacs itself.'
      +' If you do not have Emacs installed'
      +' it can tell you where to find the binary'
      +' distribution for MS Windows and help you unpack.'
      + #13#10
      + #13#10
      +'The following can also be done by this setup program:'
      ;
   end;
   LblNew.Caption := ''
   +'EmacsW32 is distributed in two type of setup programs:'
   + #13#10
   + #13#10
   +'  - EmacsW32 Setup Helper, without Emacs itself (<1MB)'
   + #13#10
   +'  - Emacs+EmacsW32 Installers, including Emacs itself (12-25MB)'
   + #13#10
   + #13#10
   + #13#10
   + sHasEmacs
   + #13#10
   + #13#10
   +'  - Windows integration of EmacsClient (shortcuts, file assoc etc)'
   + #13#10
   +'  - Install basic EmacsW32 files (includes quick printing)'
   + #13#10
   + #13#10
   +'(If you choose not to do everything now you can run this setup program again later.)'
   + #13#10
   ;

   lblURL2Label := TNewStaticText.Create(Form);
   lblURL2Label.Parent := Form;
   //LblURL2Label.Font.Color := clMaroon;
   lblURL2Label.Caption := 'For more info see ';
   //lblURL2Label.Top := URLLabel.Top + URLLabel.Height + 10;
   lblURL2Label.Top := LblNew.Top + LblNew.Height + 16;
   lblURL2Label.Left := LblNew.left;
   lblURL2Label.AutoSize := true;

   URL2Label := TNewStaticText.Create(Form);
   URL2Label.Parent := Form;
   URL2Label.Top := lblURL2Label.Top;
   URL2Label.Left := lblHdr.Left + lblURL2Label.width;
   URL2Label.Caption := 'http://OurComments.org/Emacs/EmacsW32.html';
   URL2Label.Font.Style := URL2Label.Font.Style + [fsUnderLine];
   URL2Label.Font.Color := clBlue;
   URL2Label.Cursor := crHand;
   //URL2Label.OnClick := @URL2LabelOnClick;
   URL2Label.OnClick := @URLLabelOnClick;
   { Tabstops and labels does not work well, do not know how to click }
   URL2Label.TabStop := false;
   lblURL2Label.FocusControl := URL2Label;


   { Help for installaion }
   lblURL3Label := TNewStaticText.Create(Form);
   lblURL3Label.Parent := Form;
   lblURL3Label.Caption := 'See also ';
   lblURL3Label.Top := LblURL2Label.Top + LblURL2Label.Height + 6;
   lblURL3Label.Left := LblNew.left;
   lblURL3Label.AutoSize := true;

   URL3Label := TNewStaticText.Create(Form);
   URL3Label.Parent := Form;
   URL3Label.Top := lblURL3Label.Top;
   URL3Label.Left := lblHdr.Left + lblURL3Label.width;
   URL3Label.Caption := 'Help for Installation';
   URL3Label.Font.Style := URL3Label.Font.Style + [fsUnderLine];
   URL3Label.Font.Color := clBlue;
   URL3Label.Cursor := crHand;
   URL3Label.OnClick := @HelpForInstallationOnClick;
   { URL3Label.OnKeyPress := @HelpForInstallationOnKeyPress; }
   URL3Label.TabStop := false;
   lblURL3Label.FocusControl := URL3Label;



   Button := TButton.Create(Form);
   Button.Parent := Form;
   Button.Left := Form.ClientWidth - Button.Width - 10;
   Button.Top := Form.ClientHeight - Button.Height - 10;
   Button.Caption := 'Close';
   Button.Cancel := True;
   Button.ModalResult := mrOk;

   Form.ActiveControl := Button;

   Form.ShowModal();
   Form.Release();
end; { ShowHelpForm }




/////////////////////////////////////////////////////////
//// Button Events etc

function ShouldSkipPage(PageID : integer): boolean;
var
   bSetHome : boolean;
   PageHome : TInputOptionWizardPage;
begin
   result := false;
   if (0 < Length(g_sErrFin)) then begin
      if (PageID = g_ErrPageID) then begin
         g_lblErrFin.Caption := g_sErrFin;
         result := false;
      end else begin
         result := true;
      end;
      exit;
   end;
   case PageID of
     g_ErrPageID          : begin
        result := true; {ShouldSkip4ErrPage();}
     end;
     wpSelectProgramGroup : begin
                               result := not DidSelWinShcut();
                            end;
     g_SelEmacsPageID     : begin
        result := ShouldSkip4SelEmacsPage();
     end;
     g_AppDirPageID   : begin
        result := not DidSelUnpacking();
     end;
     g_TarGzPageID    : begin
        result := ShouldSkip4TarGzPage();
     end;
     g_VanillaPageID  : begin
        result := ShouldSkip4VanillaSubDirPage();
     end;
     g_FtypePageID    : begin
        result := not DidSelAssoc();
     end;
{      g_HomePageID     : begin }
{         if DidSelHomeTask() and false then begin }
{            //msgbox2('homepageid basic'); }
{            result := not g_bHomeNotSet; }
{            //if result then msgbox2('skip HomePage ') else msgbox2('NOT skip HomePage'); }
{       end else begin }
{            //msgbox2('homepageid NOT basic'); }
{            result := true; }
{       end; }
{      end; }
{      g_SetHomePageID  : begin }
{         if DidSelHomeTask() then begin }
{          if g_bHomeNotSet then begin }
{             PageHome := TInputOptionWizardPage( PageFromID(g_HomePageID) ); }
{             bSetHome := not PageHome.Values[0]; }
{             result := (not bSetHome); }
{             //if result then msgbox2('skip SetHomePage ') else msgbox2('NOT skip SetHomePage'); }
{          end else begin }
{             result := true; }
{          end; }
{       end else begin }
{          result := true; }
{       end; }
{      end; }
   end; { case }
end; { ShouldSkipPage }

function NextOnSelectComponents(): boolean;
begin
   result := false;
   { if not (DidSelUnpacking() or g_bIncludesEmacs or DidSelBasicSetup() or DidSelAnyCustomization()) }
   if not (DidSelUnpacking() or DidSelEmacsItself() or DidSelBasicSetup() or DidSelAnyCustomization())
      {  or DidSelHomeTask()) }
   then begin
      msgbox2('Nothing to do. Please select what to do.');
      exit;
   end;
{    if DidSelAnyCustomization() then begin }
{       {\ if DidSelUnpacking or g_bIncludesEmacs then begin !{\ }
{       if DidSelUnpacking or DidSelEmacsItself() then begin }
{          if not DidSelBasicSetup() then begin }
{             MsgCritical('CustomNeedsBase'); }
{             exit; }
{          end; }
{       end; }
{       if (not DidSelBasicSetup()) and (not IsBasicSetupDoneBefore()) then begin }
{          MsgCritical('CustomNeedsBase'); }
{          exit; }
{       end; }
{    end; }
   { if g_bIncludesEmacs or DidSelUnpacking() then begin }
{    if DidSelEmacsItself() or DidSelUnpacking() then begin }
{       if not DidSelBasicSetup() then begin }
{          if MsgQuery('UnpackEmacsNoBasic', MB_YESNO) = IDYES then }
{             exit; }
{       end; }
{    end; }
   if DidSelBasicSetup() or DidSelAnyCustomization() then begin
      g_iWriteToRegRoot := HKCU;
      g_bForAllUsers    := false;
      if IsAdminLoggedOn() then begin
         if not WizardSilent() then begin
            if MsgQuery('ForAllUsers', MB_YESNO) = IDYES then begin
               g_bForAllUsers := true;
               if MsgQuery('ForAllUsersDelHKCU', MB_YESNO) = IDYES then begin
                  g_bForAllUsersDelHKCU := true;
               end;
            end;
         end;
         if g_bForAllUsers then begin
            g_iWriteToRegRoot := HKLM;
            if g_iCurrentRegRoot = HKCU then begin
               { MsgWarning('WarnDelEmacsHKCU'); }
            end;
         end else begin
            g_iWriteToRegRoot := HKCU;
         end;
      end;
   end;
   result := true;
end; { NextOnSelectComponents }

procedure HelpButtonOnClick(Sender: TObject);
begin
   ShowHelpForm();
end;

function BackButtonClick(CurPage: Integer): Boolean;
begin
   Result := true; //ScriptDlgPages(CurPage, True);
end;

procedure BrowseButtonOnClick(Sender: TObject);
var
   sInitDir : string;
begin
   sInitDir := ExtractFilePath(g_teditTarGz.Text);
   if BrowseForEmacsTarGz(sInitDir) then begin
      g_teditTarGz.Text := g_sEmacsTarGz;
   end;
end;

procedure CancelButtonClick(CurPageID: Integer; var Cancel, Confirm: Boolean);
begin
   if (CurPageID = g_ErrPageId) then begin
      Confirm := False;
   end;
end; { CancelButtonClick }

function NextButtonClick(CurPage: Integer): Boolean;
begin
   case CurPage of
     wpSelectComponents : begin
                             result :=  NextOnSelectComponents();
                          end;
     g_ErrPageId        : begin
        result := NextOnErrPage();
     end;
     g_SelEmacsPageID   : begin
        result := NextOnSelEmacsPage();
     end;
     g_AppDirPageID     : begin
        result := NextOnAppDirPage();
     end;
     g_VanillaPageID    : begin
        result := NextOnVanillaSubDirPage();
     end;
     g_TarGzPageID      :  begin
        result := NextOnTarGzPage();
     end;
{      g_HomePageID       : begin }
{         Result := NextOnHomePage; }
{      end; }
{      g_SetHomePageID : begin }
{       Result := NextOnSetHomePage; }
{      end; }
     g_FTypePageID      : begin
        result := NextOnFtypePage();
     end;
   else
      result := true;
   end; { case }
end; { NextButtonClick }

procedure CurPageChanged(CurPageID: Integer);
begin
   g_iCurPageID := CurPageId;
   case CurPageID of
     wpFinished : begin
                     Sleep(500);
                     BringToFrontAndRestore();
                  end;
   end; { case }
end; { CurPageChanged }





procedure TestButtonOnClick(Sender: TObject);
var sExtList      : string;
var tstrlistFtype : TStringList;
var atstrlistExt  : array of TStringList;
var ss            : string;
var nn : integer;
begin
   sExtList := ''
   +'.txt,.c,.h,.cc,.cpp,.hpp,.cxx,.hxx,.pas,.pl,.pm,.bas,.cls,.dcl,.idl,.pro,.java,.m4,.mak,.tex,.xml,.sgml,.sql'
   +',.html'
   ;
   //FindExtAndFtype(sExtList, tstrlistFtype, atstrlistExt);
   ss:='';
   for nn:= 0 to tstrlistFtype.Count-1 do begin
      ss := ss+#13#10#13#10+tstrlistFtype.Strings[nn];
      ss := ss+': ';
      ss := ss+atstrlistExt[nn].CommaText;
      ss := ss+#13#10+'open='+findFtypeAction(tstrlistFtype.Strings[nn],'open');
      ss := ss+#13#10+'edit='+findFtypeAction(tstrlistFtype.Strings[nn],'edit');
   end;
   msgbox(ss, mbinformation, mb_ok);
end;


procedure URLLabelOnClick(Sender: TObject);
var
   iRes       : integer;
   stxtSender : TNewStaticText;
begin
   stxtSender := TNewStaticText(Sender);
   if not ShellExec('open', stxtSender.Caption, '', '', SW_SHOWNORMAL, ewNoWait, iRes) then
   begin
      InternalErrorWithCode(iRes,
                            '(when trying to start web browser to show '
                            +stxtSender.Caption+')');
   end;
end;

procedure URL2LabelOnClick(Sender: TObject);
var
   iRes       : integer;
   sHtmlFile  : string;
   //sExtracted : string;
begin
   {
   sHtmlFile := 'emacsw32.html';
   try
      ExtractTemporaryFile(sHtmlFile);
   except
      CriticalErrorEx2('FailedExtractTmp', sHtmlFile, GetExceptionMessage());
      exit;
   end;
   sHtmlFile := 'EmacsSetupUtilities.html';
   try
      ExtractTemporaryFile(sHtmlFile);
   except
      CriticalErrorEx2('FailedExtractTmp', sHtmlFile, GetExceptionMessage());
      exit;
   end;
   sExtracted := AddBackSlash(ExpandConstant('{tmp'))+sHtmlFile;
   if not FileExists(sExtracted) then begin
         MsgCriticalEx('ExtrTmpNo  tFound', sHtmlFile);
      exit;
   end;
   if not ShellExec('open', sExtracted, '', '', SW_SHOWNORMAL, ewNoWait, iRes) then begin
      InternalErrorWithCode(iRes,
                            '(when trying to start web browser to show '+sExtracted+')');
   end;
   }
   sHtmlFile  := 'http://www.emacswiki.org/cgi-bin/wiki/EmacsW32';
   if not ShellExec('open', sHtmlFile, '', '', SW_SHOWNORMAL, ewNoWait, iRes) then begin
      InternalErrorWithCode(iRes,
                            '(when trying to start web browser to show '+sHtmlFile+')');
   end;
end;



/////////////////////////////////////////////////////////
//// Wizard Events


procedure CurStepChanged(CurStep : TSetupStep);
var
   iTot         : integer;
   iDone        : integer;
   iHome        : integer;
   iUnpackEmacs : integer;
   iAssoc       : integer;
   iBasic       : integer;
   iCustom      : integer;
begin
   case CurStep of
     //ssCopy :
     //csFinished :
     ssPostInstall :
                  begin
                     { g_sErrFin := 'testing fin error'; exit; }
                     iHome := 1;
                     iUnpackEmacs := 100;
                     iAssoc := 2;
                     iBasic := 10;
                     iCustom := 10;
                     iDone := 5;
                     iTot := iDone;
                     if g_sNewHomeDir <> '' then iTot := iTot+iHome;
                     //if DidSelUnpacking() then iTot := iTot+iUnpackEmacs;
                     if DidSelBasicSetup() then iTot := iTot+iBasic;
                     if not g_bSkipAssoc then begin
                        if DidSelAssoc() then iTot := iTot+iAssoc;
                     end;

                     if g_sNewHomeDir <> '' then begin
                        //msgbox2('before createhomedir');
                        if CreateHomeDir(g_sNewHomeDir)then begin
                           //msgbox2('before addhometoenv');
                           if not AddHomeToEnv(g_sNewHomeDir) then begin
                              CriticalError('CantAddHomeEnv');
                           end;
                        end else begin
                           CriticalErrorEx('CantCreateHomeDir', g_sNewHomeDir);
                        end;
                        iDone := iDone + iHome;
                     end;
                     if DidSelBasicSetup() then begin
                        if (g_iCurrentRegRoot = HKCU) and (g_iWriteToRegRoot = HKLM) then begin
                           if not DeleteHKCUEmacsKey() then begin
                              MsgWarning('WarnFailedDelEmacsHKCU');
                              exit;
                           end;
                           iDone := iDone + iBasic;
                        end;
                        if not LispSetupBase() then exit;
                     end;
                     if not g_bSkipAssoc then begin
                        if DidSelAssoc() then begin
                           DoAssChg();
                           iDone := iDone + iAssoc;
                        end;
                     end;
                  end;
     ssPostInstall :
                    BringToFrontAndRestore();
   end; { case }
end; { CurStepChanged }


function InitializeSetup()    : Boolean;
begin
   {
   if not UsingWinNT() then begin
      MsgBox('Sorry, You must be running Windows NT (2000 or XP) to install this',
             mbCriticalError, MB_OK);
      exit;
   end;
   }

   g_bHomeNotSet := not FindEmacsUserHome(g_sHomeDir, g_sHomeHow);
   //msgbox2(g_sHomeHow+'='+g_sHomeDir);

   if not GetEmacsDirFromReg() then begin
      GetEmacsDirFromPath();
   end;

   Result := true;
end; { InitializeSetup }


procedure InitializeWizard();
var
   HelpButton, CancelButton: TButton;
   ThisID : integer;
begin
   CancelButton := WizardForm.CancelButton;

   HelpButton := TButton.Create(WizardForm);
   HelpButton.Left := WizardForm.ClientWidth - CancelButton.Left - CancelButton.Width;
   HelpButton.Top := CancelButton.Top;
   HelpButton.Width := CancelButton.Width;
   HelpButton.Height := CancelButton.Height;
   HelpButton.Caption := '&Help...';
   HelpButton.OnClick := @HelpButtonOnClick;
   HelpButton.Parent := WizardForm;

   ThisID := wpSelectComponents;
{    ThisID := CreateHomePage(ThisId); }
{    ThisID := CreateSetHomePage(ThisID); }
   ThisID := CreateSelEmacsPage(ThisID);
   ThisID := CreateAppDirPage(ThisID);
   ThisID := CreateTarGzPage(ThisID);
   ThisID := CreateVanillaSubDirPage(ThisID);
   ThisID := CreateFtypePage(ThisID);
   CreateErrPage(wpInstalling);
end; { InitializeWizard }

function UpdateReadyMemo(Space, NewLine, MemoUserInfoInfo, MemoDirInfo, MemoTypeInfo,
                         MemoComponentsInfo, MemoGroupInfo, MemoTasksInfo: String): String;
var
   ss  : string;
   //si  : string;
   //si1 : string;
   ii  : integer;
begin
   //msgbox2('enter UpdateReadyMemo');
   if Length(MemoUserInfoInfo)   > 0 then ss := ss + MemoUserInfoInfo   +NewLine+NewLine;
   if not DidSelEmacsItself() then begin
      if Length(MemoDirInfo) > 0 then begin
         MemoDirInfo := 'Note: Will not install Emacs itself';
      end else begin
         MemoDirInfo := '';
      end;
   end;
   if Length(MemoDirInfo)        > 0 then ss := ss + MemoDirInfo        +NewLine+NewLine;
   if Length(MemoTypeInfo)       > 0 then ss := ss + MemoTypeInfo       +NewLine+NewLine;
   if Length(MemoComponentsInfo) > 0 then ss := ss + MemoComponentsInfo +NewLine+NewLine;
   if Length(MemoGroupInfo)      > 0 then ss := ss + MemoGroupInfo      +NewLine+NewLine;
   if Length(MemoTasksInfo)      > 0 then ss := ss + MemoTasksInfo      +NewLine+NewLine;
   if Length(MemoUserInfoInfo)   > 0 then ss := ss + MemoUserInfoInfo   +NewLine+NewLine;
   if DidSelBasicSetup() then begin
      ss := ss + 'Basic EmacsW32 setup will be done for Emacs in "'+emacs_dir_ToUse()+'"'+NewLine;
      ss := ss + Space + 'Some subdirectories of "'
      + ExtractFileDir(emacs_dir_ToUse())+'" will then be added or changed'+NewLine+NewLine;
   end;
   if g_sNewHomeDir <> '' then begin
      ss := ss + 'User HOME environment variable will be set to'+NewLine;
      ss := ss + Space + g_sNewHomeDir;
   end;
   if DidSelAssoc() then begin
      { if g_nAssChk > 0 then begin }
      if not g_bSkipAssoc then begin
         ss := ss + 'Moving File associations to the emacsclientw.exe:';
         for ii := 0 to g_nAssChk - 1 do begin
            if g_bAssChk[ii] then begin
               ss := ss + NewLine;
               if AssIdxIsFreeExt(ii) then begin
                  ss := ss + Space + 'Free file extension: ' + AssIdxToExt(ii);
               end else if AssIdxIsVerb(ii) then begin
                  ss := ss + Space + 'Change verb "' + AssIdxToVerb(ii)
                  + '" for file type "' + AssIdxToFtype(ii)+'"';
                  ss := ss + NewLine;
                  ss := ss + Space + Space + 'Old Action: ' + AssIdxToAct(ii);
                  ss := ss + NewLine;
                  ss := ss + Space + Space + 'New Action: ' + AssIdxToNewAct(ii);
               end else begin
                  ss := ss + Space + 'File extention associated with file type "'
                  + AssIdxToFtype(ii)+'": ' + AssIdxToExt(ii);
               end;
            end; { checked }
         end; { for }
      end;
   end; { File assoc }
   result := ss;
end; { UpdateReadyMemo }



{********** Here because Emacs pascal-mode does not recognize try, except, end *******}
function StrToIntProtected(str    : string;
                           defalt : integer): integer;
var i : integer;
begin
   i := defalt;
   try
   i:= StrToInt(str);
   except
end;
end; { StrToIntProtected }

{*** End of CODE **}




