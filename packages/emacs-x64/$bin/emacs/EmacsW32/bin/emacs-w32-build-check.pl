# Copyrigh 2005 Lennart Borgman. All rights reserved.

use strict;

#Fix-me: is a special check for makeinfo requires when MSYS is used????
# Or was this a misunderstanding from the beginning???

my @errors;
my @path = split(";", $ENV{Path});

my $using_sh;
my %progs_needed = (
		    "cvs"      => [ [1,11,20], "CVS Concurrent Versions System"],
		    "cp"       => [ [4,1],    "GNU cp" ],
		    "rm"       => [ [4,1],    "GNU rm" ],
		    "makeinfo" => [ [4,6],    "GNU makeinfo"], # lisp\makefile
		    "mv"       => [ [4,1],    "GNU mv"],       # lisp\makefile
		    "touch"    => [ [4,1],    "GNU touch"],    # lisp\makefile
                    # find and xargs does not seem to be needed any more (when using cmd.exe) - 2005-07-02 
		    #"find"     => [ [4,1],    "GNU find"],     # lisp\makefile
		    #"xargs"    => [ [4,1],    "GNU xargs"],    # lisp\makefile
	     );
my $plink = $ENV{"CVS_RSH"};
my %progs_needed_paths;
my @progs_needed_missing;

sub print_welcome() {
    print "\n*** Starting checks for sane build environment for Emacs ***\n";
    print "(See w32-build-emacs.html that comes with EmacsW32 for more info.)\n\n";
} # print_welcome

sub get_prog_path($) {
    my $prog = shift;
    for my $dir (@path) {
	$dir =~ tr|\\|/|;
	if (substr($dir,-1) ne '/') { $dir .= '/'; }
	my $file = $dir . $prog . ".exe";
	if (-e $file) {
	    #print "OK $file\n";
	    return $dir;
	}
    }
    return undef;
} # get_prog_path

sub check_version($$) {
    my $ver = shift;
    my @ver = @$ver;
    my $need_ver = shift;
    my @need_ver = @$need_ver;
    for my $ii (0 .. scalar(@need_ver)-1) {
	return unless $ii < scalar @ver;
	return 1 if $ver[$ii] > $need_ver[$ii];
	return unless $ver[$ii] >= $need_ver[$ii];
    }
    return  1;
} # check_version

sub check_version_with_error($$$) {
    my $ver = shift;
    my $need_ver = shift;
    return 1 if check_version($ver, $need_ver);
    my $prog = shift;
    push @errors, "$prog version ("
	. join(".", @$ver)
	. ") too low - need "
	. join(".", @$need_ver);
} # check_version_with_error

sub get_sh_info() {
    print "Checking shell and special programs...\n";
    my $sh_dir = get_prog_path("sh");
    my $cl_dir = get_prog_path("cl");
#     if ($sh_dir && length($cl_dir) == 0) {
# 	push @errors, "sh.exe must not be in your path when you use gcc";
# 	exit;
#     }
    if ($cl_dir) {
	# This will be used first by configure
	print "  Using MSVC\n";
	my $cl_verstr = qx(cl 2>&1);
	print "  Using cmd.exe as shell and nmake as make\n";
	my @ver = ($cl_verstr =~ /(\d+)\.(\d+)\.(\d+)/);
	if (check_version_with_error(\@ver, [2, 0, 8804], "MSVC cl")) {
	    print "  MSVC cl version OK\n";
	}
    } else {
	$progs_needed{"make"} = [ [3,80], "GNU Make" ];
	$progs_needed{"gcc"} = [ [3,2,3], "GNU C/C++ Compiler" ];
	my $make_dir = get_prog_path("make");
	my $make_verstr;
	my $is_msys_make;
	my $is_cygwin_make;
	if ($make_dir) {
	    $make_verstr = qx(make --version);
            $is_msys_make = ($make_verstr =~ /msys/im);
            $is_cygwin_make = ($make_verstr =~ /cygwin/im); # fix-me: check this!
	}
	if ($sh_dir) {
	    push(@{$progs_needed_paths{$sh_dir}}, "sh" );
	    # check if MSYS:
	    my $sh_verstr = qx(sh --version);
	    my $is_msys_sh = ($sh_verstr =~ /msys/im);
	    if ($is_msys_sh) {
		print "  Using MSYS sh\n";
		$using_sh = "MSYS";
		my @ver = ($sh_verstr =~ /(\d+)\.(\d+)\.(\d+)/);
		if (check_version_with_error(\@ver, [2, 4, 0], "MSYS sh")) {
		    print "  sh version OK\n";
		}
		if ($is_msys_make) {
		    print "  Using MSYS make (OK)\n";
		} else {
		    push @errors, "Using MSYS sh but not MSYS make";
		}
	    } else {
		# check if Cygwin:
		my $is_cygwin_sh = ($sh_verstr =~ /cygwin/im); # Fix-me: is this correct???
		if ($is_cygwin_sh) {
		    print "  Using Cygwin sh\n";
		    $using_sh = "Cygwin";
		    # Fix-me: check version ok!
		    #my @ver = ($sh_verstr =~ /(\d+)\.(\d+)\.(\d+)/);
		    #if ((check_version(\@ver, [2, 4, 0])) {

		    my $make_verstr = qx(make --version);
		    if ($is_cygwin_make) {
			print "  Using Cygwin make (OK)\n";
		    } else {
			push @errors, "Using Cygwin sh but not Cygwin make";
		    }
		} else {
		    $using_sh = "Unknown";
		    print "  Using unknown sh\n";
		}
	    }
	} else {
	    if ($is_cygwin_make) {
		push @errors, "Cygwin make can only be used with Cygwin";
	    }
	    if ($is_msys_make) {
		push @errors, "MSYS make can only be used with MSYS";
	    }
	}
    }
} # get_sh_info


sub check_needed() {
    print "Checking needed programs existence and location\n";
    for my $prog (keys %progs_needed) {
	my $dir = get_prog_path($prog);
	if ($dir) {
	    my $verstr = qx($prog --version);
	    my @ver = ($verstr =~ /(\d+)(?:\.(\d+))(?:\.(\d+))?(?:\.(\d+))?/);
	    while (! defined $ver[$#ver]) { pop @ver; }
	    my @args = @{$progs_needed{$prog}};
	    check_version_with_error(\@ver, $args[0], $args[1]);
	    push(@{$progs_needed_paths{$dir}}, $prog );
	} else {
	    push(@progs_needed_missing, $prog);
	}
    }
    #my $plink = $ENV{"CVS_RSH"};
    if ($plink && $plink =~ /plink\.exe$/i) {
	if (! -x $plink) {
	    push @errors, "Can not find CVS_RSH prog ($plink)";
	}
    } else {
	push @errors, "Environment variable CVS_RSH is not pointing to plink.exe";
    }
    my $find_verstr = qx("find" --help 2>&1);
    if ($?) {
	push @errors, "You need to have unix find.exe before MS Windows find.exe in the Path";
    }
} # check_needed

sub tell_results() {
    print "\n*** Results ***\n";
#     if ($using_sh) {
# 	if (scalar(keys %progs_needed_paths) == 1) {
# 	    my $dir = (keys %progs_needed_paths)[0];
# 	    print "All needed progs in same dir ($dir)\n";
# 	} else {
# 	    print "WARNING: Not all needed programs are in the same directory:\n";
# 	    for my $dir (keys %progs_needed_paths) {
# 		printf "  %-20s\t@{$progs_needed_paths{$dir}}\n", "$dir:";
# 	    }
# 	}
#     }
    print "Program locations:\n";
    for my $dir (keys %progs_needed_paths) {
	printf "  %-20s\t@{$progs_needed_paths{$dir}}\n", "$dir:";
    }
    #my $plink = $ENV{"CVS_RSH"};
    printf "  %-20s\t%s\n", "CVS_RSH", $plink ? $plink : "(not set)"; #$ENV{"CVS_RSH"};

    if (scalar(@progs_needed_missing) > 0) {
	printf "WARNING: Missing progs: (@progs_needed_missing)\n";
    }
    if (scalar @errors > 0 ) {
	for my $err (@errors) {
	    print "ERROR: $err\n";
	}
    } else {
	print "No errors found\n";
    }
} # tell_results


print_welcome();
get_sh_info();
check_needed();    
tell_results();

