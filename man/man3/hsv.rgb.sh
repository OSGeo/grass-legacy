.TH hsv.rgb.sh
.SH NAME
\fIhsv.rgb.sh\fR \- Converts HSV (hue, saturation, and value) cell values to
RGB (red, green, and blue) values.
.br
.I "(GRASS Shell Script)"
.SH SYNOPSIS
\fBhsv.rgb.sh \fIfile1 file2 file3 \fR
.SH DESCRIPTION
.I hsv.rgb.sh
is a Bourne shell (sh(1)) program that converts HSV to RGB using
\fIr.mapcalc\fR.  The Foley and Van Dam algorithm is the basis for this 
program.
Input must be three raster files - each file supplying the HSV values.
Three new raster files are created representing RGB.
.SH NOTES
Do not use the same names for input and output.
.SH FILES
This program is simply a shell script stored in the file \fIhsv.rgb.sh\fR
under the $GISBASE/scripts directory.
Users are encouraged to make their own shell script programs using similar
techniques.
.SH SEE ALSO
.I rgb.hsv.sh
.SH AUTHOR
James Westervelt, U.S. Army Coonstruction Engineering Research Laboratory
