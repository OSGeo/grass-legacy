.TH split.sh
.SH NAME
\fIsplit.sh\fR \- Divides the graphics monitor into two frames and then
displays two maps in these frames.
.br
.I (GRASS Display Program)
.SH SYNOPSIS
\fBsplit.sh mapname mapname \fR[\fBcmd\*=\fIGRASS_command\fR] [\fBcmd2\*=\fIGRASS_command\fR] [\fBview\*=horiz\fR]
.SH DESCRIPTION
.I split.sh
is a Bourne shell (sh(1)) script that clears the entire graphics screen
and divides it into two display frames.
Map layers are then displayed in each of the two frames.
This command is very useful for visually comparing maps
(raster, vector, and 3-d views) and can be used by other GRASS shell macros.
It is also useful for creating demos.  Program parameters are given below.

\fBParameters:\fR
.IP \fBview\*=horiz\fR 18
The graphics screen can be split either horizontally or vertically.
The default view splits the screen into two frames, one on the left
and one on the right (a vertical split).  Some maps ( 3-d views) are better
represented with more width then height (horizontal split).  The first map
name listed on the command line will be displayed in the top or left window
(depending on whether the screen was split horizontally or vertically), and
The second map will be displayed in the bottom or right window.
.IP \fBcmd\*=\fIGRASS_command\fR 18
The GRASS command used to display the named \fImapname\fRs.
If no command is specified by the user, \fId.rast\fR is used by default.
However, any GRASS display command (e.g., \fId.3d, d.vect\fR, etc...)
can be entered.
.IP \fBcmd2\*=\fIGRASS_command\fR 18
This command will be used to display map data in the second frame only.
.sp
If the user fails to specify the values of both \fIcmd\fR and \fIcmd2fR,
\fIsplit.sh\fR will use the default command (\fId.rast\fR) to display
user-specified map layer names in both frames.
If the user specifies only the value of \fIcmd\fR on the command line,
then that command will be executed for both frames.
If the user specifies the values of both \fIcmd\fR and \fIcmd2\fR on the 
command line, the \fIcmd\fR command will be executed in frame 1 and the
\fIcmd2\fR command will be executed in frame 2.
.SH EXAMPLES
\fBsplit.sh  soils  vegcover\fR
.LP
\fBsplit.sh  soils  cmd2\*=d.legend "soils red"\fR
.LP
\fBsplit.sh  elevation  vegcover  cmd\*=d.3d  view\*=horiz\fR
.SH NOTES
\fIsplit.sh\fR leaves the frame that the last map was drawn in as the active
frame.  The order in which the options (\fIcmd, cmd2, view\fR) are placed
on the command line doesn't matter, but the order is important for the map
names.
.SH FILES
This program is simply a shell script.  Users are encouraged to make their own
shell scripts using similar techniques.  See $GISBASE/scripts/split.sh.
.SH "SEE ALSO"
.I d.3d,
.I d.frame,
.I d.rast,
.I d.sites,
.I d.vect
.SH "AUTHOR"
Michael Higgins, U.S. Army Construction Engineering Research Laboratory
