.TH blend.sh
.SH NAME
\fIblend.sh\fR\ \- Combines the red, green, and blue color components
of two raster map layers.
.br
.I "(GRASS Shell Script)"
.SH SYNOPSIS
.B blend.sh file1 file2 perc outbase
.SH DESCRIPTION
.I blend.sh
is a Bourne shell (sh(1)) script that extracts the red (R), green (G),
and blue (B) color components from each of two raster map layers,
and creates three new raster map layers whose category values respectively
represent the combined red, combined blue, and combined green color values
from the two input layers.
Category values in each of the output map layers will fall within the range
of 0 - 255.
.LP
The R,G,B values from the two input map layers (\fIfile1\fR and \fIfile2\fR)
are not simply added together, but are instead combined by a user-named
percentage (\fIperc\fR) of the R,G,B values in \fIfile1\fR.
Specifically, \fIblend.sh\fR executes three \fIr.mapcalc\fR statements that: 
(1) convert the R,G,B values in \fIfile1\fR and \fIfile2\fR to the range 0 - 255;
(2) multiply the R, G, and B values in \fIfile1\fR by a user-named percentage
(\fIperc\fR);
(3) multiply the R, G, and B values in \fIfile2\fR by (100 - \fIperc\fR)\*%;
(4) create three new raster map layers, whose category values represent the
summed R, summed G, or summed B values resulting from (2) and (3).
Resulting R, G, and B values will respectively be stored in three new raster map
layers named \fIoutbase\fR.r, \fIoutbase\fR.g and \fIoutbase\fR.b.
.LP
This program runs non-interactively;  the user must state all parameter values
on the command line.

.LP
\fBParameters:\fR
.IP \fIfile1\fR 18
Name of a first raster map layer, whose R, G, and B color components will
be combined with those of the second raster map layer (\fIfile2\fR) named.
The percent value (\fIperc\fR) given will apply to \fIfile1\fR.
.IP \fIfile2\fR 18
Name of a second raster map layer, whose color components will be combined with
those of \fIfile1\fR.
The percent value (\fIperc\fR) given will apply to the R,G,B values in \fIfile1\fR.
The R, G, and B values in \fIfile2\fR will be multiplied by (100 - \fIperc\fR) %.
.IP \fIperc\fR 18
Percentage or amount of the color contribution in terms of color intensity.
This value is multiplied by the R,G,B values in \fIfile1\fR.
.IP \fIoutbase\fR 18
The root name assigned to each of the three output files created.
A suffix is added to each file name, indicating which hold the red, green, and blue
color values.
.LP
\fIblend.sh\fR executes three \fIr.mapcalc\fR statements:
.LP
.nf
.B
.RS
r.mapcalc "\fIoutbase\fP.r = r#\fIfile1\fP * .\fIperc\fP + (1.0 - .\fIperc\fP) * r#\fIfile2\fP"
r.mapcalc "\fIoutbase\fP.g = g#\fIfile1\fP * .\fIperc\fP + (1.0 - .\fIperc\fP) * g#\fIfile2\fP"
r.mapcalc "\fIoutbase\fP.b = b#\fIfile1\fP * .\fIperc\fP + (1.0 - .\fIperc\fP) * b#\fIfile2\fP"
.RE
.R
.fi
.LP
It uses the # operator to separately extract the red, green, and blue components
in the named raster map layers, essentially allowing color separates to be made.
.SH EXAMPLE
Typing the following at the command line:
.LP
.RS
\fBblend.sh aspect elevation 40 elev.asp \fR
.RE
.LP
will create three new raster map layers named
\fIelev.asp.r\fR, \fIelev.asp.g\fR, and \fIelev.asp.b\fR,
that, respectively, contain 40\*% of the red, green, and blue components
of the \fIelevation\fR map layer
and contain 60\*% of the red, green, and blue components of the \fIaspect\fR map layer.
.SH FILES
This program is simply a shell script.  Users are encouraged to make their own
shell scripts using similar techniques.  See $GISBASE/scripts/blend.sh.
.SH SEE ALSO
.I r.colors,
.I r.mapcalc
.SH AUTHOR
Dave Gerdes, U.S. Army Construction Engineering Research Laboratory
