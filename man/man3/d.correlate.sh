.TH dcorrelate.sh
.SH NAME
.I dcorrelate.sh \- Graphically displays the correlation among from two to four
raster map layers in the active frame on the graphics monitor.
.br
\fI(GRASS Shell Script)\fR
.SH SYNOPSIS
.B dcorrelate.sh \fIlayer1 layer2 \fR[\fIlayer3\fR [\fIlayer4\fR]]
.SH DESCRIPTION
.I dcorrelate.sh
is a C-shell (csh(1)) script that graphically displays the results
of an \fIr.stats\fR run on two raster map layers.
This shell script is useful for highlighting the correlation (or lack of it)
among data layers.

The results are displayed in the active display frame on the user's graphics monitor.
\fIdcorrelate.sh\fR erases the active frame before displaying results.
.LP
\fBParameters:\fR
.IP "\fIlayer1 layer2 \fR[\fIlayer3\fR [\fIlayer4\fR]]" 18
The names of from two to four existing raster map layers
to be included in the correlation.
.SH NOTES
This is a shell script that uses \fIr.stats\fR and the UNIX \fIawk\fR command
to calculate the correlation among data layers,
and uses \fId.text\fR and \fId.graph\fR to display the results.

If three or four map layers are specified,
the correlation among each combination of two data layers is displayed.

This command is written for /bin/csh.  If your system doesn't support this shell,
don't install this script.
.SH FILES
This program is simply a shell script.
Users are encouraged to make their own shell script programs using similar techniques.
See $GISBASE/scripts/dcorrelate.sh.
.SH SEE ALSO
The UNIX \fIawk\fR command
.LP
.I d.text,
.I d.graph,
.I r.coin,
.I r.stats
.SH AUTHOR
Michael Shapiro, U.S. Army Construction Engineering Research Laboratory
