.TH show.fonts.sh
.SH NAME
\fIshow.fonts.sh\fR \- Displays and names available font types in the active
display frame on the graphics monitor.
.br
.I "(GRASS Shell Script)"
.SH SYNOPSIS
.B show.fonts.sh
.SH DESCRIPTION
.I show.fonts.sh
is a UNIX Bourne shell macro which runs the \fId.erase -a\fR command,
then names and displays the font types that can be selected using
\fId.font\fR.  This macro also runs the GRASS
commands \fId.font\fR and \fId.text\fR.  See the manual entry for
\fId.font\fR for instructions on choosing a font type.
.LP
No program arguments are required to run this program.
.SH BUGS
The font is set to
.I romans
(Roman simplex) after running
.I show.fonts.sh.
There is no mechanism to query the current font, so
there is no way to automatically restore the font.
The user will have to reset the font type using
.I d.font
if
.I romans
is not desired.
.SH FILES
This program is simply a shell script stored under the $GISBASE/scripts directory.
Users are encouraged to examine the shell script programs stored here
and to produce others for their own use.
.SH "SEE ALSO"
.I d.display,
.I d.erase,
.I d.font,
.I grass.logo.sh,
.I d.label,
.I d.legend,
.I d.paint.labels,
.I d.text,
.I d.title
.SH "AUTHOR"
James Westervelt, U.S. Army Construction Engineering Research Laboratory
