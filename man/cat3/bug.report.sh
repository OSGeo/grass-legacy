


bug.report.sh <scriptsGRASS Reference Manu<scripts> bug.report.sh



NAME
     bug.report.sh - A mechanism for writing, storing, and e-
     mailing to	USACERL	users' bug reports on GRASS 4.0	commands.
     (GRASS Shell Script)

SYNOPSIS
     bug.report.sh 4.0_program.name [program_arguments]

DESCRIPTION
     bug.report.sh is a	Bourne shell (sh(1)) script which, when
     given a GRASS 4.0 program name, allows the	user to	complete
     a bug report for that program.  Completed bug reports can be
     e-mailed to the GRASS development group at	USACERL, saved to
     a file in the user's home directory, or discarded.

     This program is not interactive;  the user	must specify the
     name of a 4.0 program on the command line.	 Program
     arguments can optionally be entered by the	user.

     Parameters:

     4.0_program.name  The name	of an existing GRASS version 4.0
		       program tested by the user.

     program_argument(s)
		       Program arguments (parameters and/or
		       flags) for the 4.0_program.name tested by
		       the user.  The user should enter	whatever
		       command arguments were actually run when
		       the program bug occurred.

EXAMPLE
     For example, if the user wished to	complete a bug report on
     v.to.rast after running the program, the user might then
     type:

	  bug.report.sh	v.to.rast

     A standard	bug report form	would then be displayed	on the
     user's text terminal, containing the user's current GRASS
     region settings, and the machine name, GRASS data base,
     location, and mapset, on which the	user is	currently running
     GRASS.  Program parameters	and flag settings, and the
     command entered by	the user on the	command	line, are also
     automatically entered on the bug report form.  The	user is
     put into a	text editor and	expected to enter additional
     information describing the	nature of the bug found	or the
     results of	program	testing.

     The user is then asked whether the	completed bug report is
     to	be:
	  1 - mailed to	westerve@zorro.cecer.army.mil



GRASS 4.2		Baylor University			1






bug.report.sh <scriptsGRASS Reference Manu<scripts> bug.report.sh



	  2 - added to the file	"grass.bugs" in	the user's home
	  directory
	  3 - both 1 and 2
	  4 - thrown away

NOTES
     This program prints whatever region settings, GRASS data
     base, location, and mapset	are current when the user runs
     bug.report.sh.  The user is therefore advised to run
     bug.report.sh immediately after experiencing a program bug,
     to	ensure that the	settings current when the bug occurred
     are reported on the bug report form.

FILES
     This shell	script is stored under the $GISBASE/scripts
     directory on the user's system.  The user is encouraged to
     examine the shell script commands stored in this directory
     and to produce similar scripts for	their own use.	Users
     might modify this shell script to e-mail reports of program
     bugs to a local systems' administrator in addition	to
     someone at	USACERL.

AUTHOR
     James Westervelt, U.S. Army Construction Engineering
     Research Laboratory






























GRASS 4.2		Baylor University			2



