.TH old.cmd.sh
.SH NAME
\fIold.cmd.sh\fR \- Provides the new GRASS version 4.0 program name for any
program name in GRASS version 3.2.
.br
.I "(GRASS Shell Script)"
.SH SYNOPSIS
\fBold.cmd.sh \fI3.2_program.name\fR
.SH DESCRIPTION
.I old.cmd.sh
is a Bourne shell (sh(1)) script which, when given a GRASS 3.2 program name,
returns the name of the new GRASS version 4.0 program performing its functions.
This program is useful as a quick on\-line cross-reference between
GRASS versions 3 and 4.
This program is not interactive;  the user must specify the name of a
3.2 program on the command line.

.LP
\fBParameter:\fR
.IP \fI3.2_program.name\fR 18
The name of an old GRASS version 3.2 program.
 
.LP
Output will be in the form:
.LP
.RS
\fIold 3.2 program name\fR  replaced with:  \fInew 4.0 program name\fR
.RE
.SH EXAMPLE
For example, to learn which GRASS 4.0 command performs the 
function of the GRASS 3.2 \fIlist\fR command, the user might type:
.LP
.RS
\fBold.cmd.sh  list \fR
.RE
.LP
The user would then see the following message displayed to standard output:
.LP
.RS
\fIlist\fR  replaced with:  \fIg.list\fR 
.RE
.SH FILES
This shell script is stored under the $GISBASE/scripts directory on the user's system.
The user is encouraged to examine the shell script commands stored in this
directory and to produce similar scripts for their own use.
.SH AUTHOR
James Westervelt, U.S. Army Construction Engineering Research Laboratory
