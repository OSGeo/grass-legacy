.TH start.man.sh
.SH NAME
\fIstart.man.sh\fR \- Creates the template for a manual entry in standard
User's Reference Manual format for a user-specified GRASS 4.0 command.
.br
.I "(GRASS Shell Script)"
.SH SYNOPSIS
\fBstart.man.sh \fI4.0_program.name\fR
.SH DESCRIPTION
.I start.man.sh
is a Bourne shell (sh(1)) script which, when given a GRASS 4.0 program name,
creates a basic manual entry for that program in the same standard format
as that used by the GRASS User's Reference Manual.  The named program
must already exist under a directory for GRASS main, alpha, or contributed
source code.
.LP
This program is not interactive;  the user must specify the name of a
4.0 program on the command line.
.LP
By default, program output will be sent to standard output
(i.e., displayed to the user's text terminal).
If the user wishes to save the manual entry created by \fIstart.man.sh\fR,
program output can be redirected into a file.
For example, the below command will create
a manual entry for the program \fInew.program\fR, and save output to the
file \fInew.program.man\fR in the user's current directory.
.LP
.RS
\fBstart.man.sh new.program > new.program.man
.RE
.LP
\fBParameter:\fR
.IP \fI4.0_program.name\fR 18
The name of an existing GRASS program located in a source code directory
for main, alpha, or contributed software.
.SH FILES
This shell script is stored under the $GISBASE/scripts directory on the user's system.
The user is encouraged to examine the shell script commands stored in this
directory and to produce similar scripts for their own use.
.SH SEE ALSO
\fBGRASS 4.0 User's Reference Manual\fR,
by Jim Westervelt, Michael Shapiro, et al (USACERL).
.LP
.I bug.report.sh,
.I g.help,
.I g.manual
.SH AUTHOR
James Westervelt, U.S. Army Construction Engineering Research Laboratory
