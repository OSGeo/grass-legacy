


old.cmd.sh <scripts> GRASS Reference Manual  <scripts> old.cmd.sh



NAME
     old.cmd.sh	- Provides the new GRASS version 4.0 program name
     for any program name in GRASS version 3.2.
     (GRASS Shell Script)

SYNOPSIS
     old.cmd.sh	3.2_program.name

DESCRIPTION
     old.cmd.sh	is a Bourne shell (sh(1)) script which,	when
     given a GRASS 3.2 program name, returns the name of the new
     GRASS version 4.0 program performing its functions.  This
     program is	useful as a quick on-line cross-reference between
     GRASS versions 3 and 4.  This program is not interactive;
     the user must specify the name of a 3.2 program on	the
     command line.


     Parameter:

     3.2_program.name  The name	of an old GRASS	version	3.2
		       program.


     Output will be in the form:

	  old 3.2 program name	replaced with:	new 4.0	program
	  name

EXAMPLE
     For example, to learn which GRASS 4.0 command performs the
     function of the GRASS 3.2 list command, the user might type:

	  old.cmd.sh  list

     The user would then see the following message displayed to
     standard output:

	  list	replaced with:	g.list

FILES
     This shell	script is stored under the $GISBASE/scripts
     directory on the user's system.  The user is encouraged to
     examine the shell script commands stored in this directory
     and to produce similar scripts for	their own use.

AUTHOR
     James Westervelt, U.S. Army Construction Engineering
     Research Laboratory






GRASS 4.2		Baylor University			1



