


start.man.sh <scripts>GRASS Reference Manua<scripts> start.man.sh



NAME
     start.man.sh - Creates the	template for a manual entry in
     standard User's Reference Manual format for a user-specified
     GRASS 4.0 command.
     (GRASS Shell Script)

SYNOPSIS
     start.man.sh 4.0_program.name

DESCRIPTION
     start.man.sh is a Bourne shell (sh(1)) script which, when
     given a GRASS 4.0 program name, creates a basic manual entry
     for that program in the same standard format as that used by
     the GRASS User's Reference	Manual.	 The named program must
     already exist under a directory for GRASS main, alpha, or
     contributed source	code.

     This program is not interactive;  the user	must specify the
     name of a 4.0 program on the command line.

     By	default, program output	will be	sent to	standard output
     (i.e., displayed to the user's text terminal).  If	the user
     wishes to save the	manual entry created by	start.man.sh,
     program output can	be redirected into a file.  For	example,
     the below command will create a manual entry for the program
     new.program, and save output to the file new.program.man in
     the user's	current	directory.

	  start.man.sh new.program > new.program.man

     Parameter:

     4.0_program.name  The name	of an existing GRASS program
		       located in a source code	directory for
		       main, alpha, or contributed software.

FILES
     This shell	script is stored under the $GISBASE/scripts
     directory on the user's system.  The user is encouraged to
     examine the shell script commands stored in this directory
     and to produce similar scripts for	their own use.

SEE ALSO
     GRASS 4.0 User's Reference	Manual,	by Jim Westervelt,
     Michael Shapiro, et al (USACERL).

     bug.report.sh, g.help, g.manual

AUTHOR
     James Westervelt, U.S. Army Construction Engineering
     Research Laboratory




GRASS 4.2		Baylor University			1



