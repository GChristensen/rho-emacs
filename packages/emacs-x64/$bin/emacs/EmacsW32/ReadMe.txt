ReadMe.Txt
**********

** Please observe that this is a beta-release!!! **

This readme file is for installation package creation. If you are just
using EmacsW32 then please read the files in the etc subdir instead of
this!!!.


Distribution formats
********************

This is the readme file for EmacsW32 Install Wizard and EmacsW32
Installer. These are two closely related setup utilities for EmacsW32
and Emacs on MS Windows.

Internally they are as setup utilities essentially the same, though
they are distributed in three forms. These three forms are very
different to the user:

1) EmacsW32-alone-[EmacsW32-ver].zip (about 3MB)

   This zip file contains the code for building the actual setup
   utilities.

2) EmacsW32-alone-[EmacsW32-ver].exe (about 4MB)

   This is a setup utility to help with installation of Emacs. It does
   not include Emacs itself. Emacs has to be downloaded separately in
   the form of a full binary distribution file for MS Windows Emacs
   Setup helper includes information about where to find this file.
   (This file was typically called emacs-21.3.tar.gz, but there is no
   such file for Emacs 22 yet so this does not work now).

   You can use this to install a new Emacs or to setup your current
   Emacs installation.

3) Emacs-[emacs.version]-DISTRIBID-EmacsW32-[EmacsW32-ver].exe (25MB)

   This includes Emacs itself and you need nothing else to install
   Emacs.



Instructions for Distribution Packages Creation
***********************************************

From any of the three types of distribution forms you can rebuild the
other.  This is done with .cmd-files in the setup subdirectory:

mkZip.cmd: Makes the file
    Output/EmacsW32-alone-[EmacsW32-ver].zip.

mkSetupHelper.cmd: Builds
    Output/EmacsW32-alone-[EmacsW32-ver].exe. Inno Setup is
    required, see below.

mkInstaller.cmd: This builds
    Emacs-[emacs-ver]-DISTRIBID-EmacsW32-[EmacsW32-ver].exe.  You are
    prompted for the DISTRIBID (or you can give it as a parameter to
    mkInstaller.cmd). You must also write a readme-DISTRIBID.txt. This
    will be included in the distribution.

    Inno Setup is required, see below.

    IMPORTANT: Please notice that this will take the Emacs you are
    currently running and include that in the installer. If you have
    any subdirectories to the grand-parent directory of +exec-directory+
    (the Emacs variable) then they will all be included in the
    distribution. You may for example have a site-lisp directory there
    (or even other applications and private docs, be aware!).

mkBase.cmd: Rewrites the basic setup. Also writes the command files
above.

If you want to change the version number then change all the version
numbers in EmacsW32.iss and then run mkBase.cmd.

Inno Setup
**********

   You will need Inno Setup to build Emacs Setup Helper and Emacs
   Installer. Inno Setups command line compiler iscc.exe must be in
   your path.

   Inno Setup is a free installer for MS Windows and can be downloaded
   from http://www.jrsoftware.org/. Note that you must use version
   4.2.6, earlier versions will not work. The next version of Inno
   Setup (version 5, currently in early beta) will not work either
   because much if handling of the "Wizard pages" have been
   rewritten. (I hope to rewrite these setup utilities as soon as
   version 5 of Inno Setup is released.)



Redistribution
**************

Please do not redistribute currently without my permission!



Acknowledgements
****************

Emacs Setup Helper and Emacs Installer are built using the excellent
Inno Setup free installer for MS Windows. You can find more info about
Inno Setup at Jordan Russels web site http://www.jrsoftware.org/.

With the permission of Ivan Pavlov 7za.exe from 7-zip is included for
unpacking distribution files. 7-zip is an advanced and free "zip
program". You can find more information at http://www.7-zip.org/.
