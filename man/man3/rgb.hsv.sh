.TH rgb.hsv.sh
.SH NAME
\fIrgb.hsv.sh\fR\ \- Converts RGB (red, green, and blue) cell values
to RGB (red, green, and blue) values.
.br
.I "(GRASS Shell Script)"
.SH SYNOPSIS
\fBrgb.hsv.sh \fIfile1 file2 file3\fR
.SH DESCRIPTION
.I rgb.hsv.sh
is a Bourne shell (sh(1)) program which converts RGB values to HSV using
\fIr.mapcalc\fR.  The Foley and Van Dam algorithm is the basis for the
program.
Input must be three raster files - each file supplying the RGB values.
Three new raster files are created representing HSV.
.SH NOTES
Do not use the same names for input and output.
.SH FILES
This program is simply a shell script stored under the $GISBASE/scripts
directory.  The user is encouraged to examine the shell script programs stored here
and to produce other such programs.
.SH SEE ALSO
.I hsv.rgb.sh
.SH AUTHOR
James Westervelt, U.S. Army Construction Engineering Research Laboratory
