.TH show.color.sh
.SH NAME
\fIshow.color.sh\fR \- Displays and names available primary colors used
by GRASS programs, in frames on the graphics monitor.
.br
.I "(GRASS Shell Script)"
.SH SYNOPSIS
.B show.color.sh
.SH DESCRIPTION
.I show.color.sh
is a UNIX Bourne shell macro that displays and names available primary colors
used by GRASS programs in frames on the graphics monitor.
Available colors are:  \fIred, orange, yellow, green, blue, indigo,\fR
\fIviolet, white, black, gray, brown, magenta\fR, and \fIaqua\fR.
.LP
No program arguments are required to run this program.
.SH NOTES
The full monitor screen is made the active frame after this program ends.
.LP
This macro is located in \fI$GISBASE/scripts/show.color.sh\fR.
.SH "SEE ALSO"
.I d.colormode,
.I d.colors,
.I d.colortable,
.I d.display,
.I d.frame,
.I grass.logo.sh,
.I show.fonts.sh
.SH "AUTHOR"
David Gerdes, U.S. Army Construction Engineering Research Laboratory
