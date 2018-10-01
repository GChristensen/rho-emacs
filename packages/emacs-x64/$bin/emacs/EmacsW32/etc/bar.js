//	Copyright 2000, Lennart Borgman. All rights reserved.

function bar_toc(opened, relpath) {

    var arr_tds = new Array();
    var i = 0;
    arr_tds[i] = td_tb(opened==i,
		       "Making Emacs easier for MS Windows users",
		       relpath+"EmacsW32Util.html",
		       "EmacsW32"
		       );
    i++;
    arr_tds[i] = td_tb(opened==i,
		       "Setting up and installing Emacs and EmacsW32",
		       relpath+"EmacsW32SetupUtilities.html",
		       "EmacsW32 Install Wizard"
		       );
    i++;
    arr_tds[i] = td_tb(opened==i,
		       "How to get Unix Utilities for Emacs on MS Windows",
		       relpath+"w32-unix-progs.html",
		       "Unix Utilities"
		       );
    i++;
    arr_tds[i] = td_tb(opened==i,
		       "Building Emacs from (CVS) sources",
		       relpath+"w32-build-emacs.html",
		       "Building Emacs"
		       );
    var tds;
    var t = 
	table('border="0" id="topmenu" align="center" cellspacing="2" ',
	      tr("", arr_tds.join(" "))
	      );
    return t;
}
