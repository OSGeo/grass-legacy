.TH r.to.gnuplot
.SH NAME
\fIr.to.gnuplot\fR \- outputs a raster map in GNUPLOT format
.br
.I "(GRASS Shell Script)"
.SH SYNOPSIS
\fBr.to.gnuplot help\fR
.br
\fBr.to.gnuplot\fR \fIname\fR
.SH DESCRIPTION
.I r.to.gnuplot
is a Bourne shell script that converts a raster map into
a format suitable for plotting with
.I g.gnuplot
and writes the results to standard output.
.SH OPTIONS
This program runs non-interactively;  the user must state all parameter values
on the command line.
.LP
\fBParameter:\fR
.IP \fIname\fR 18
Name of a raster map layer.
.SH EXAMPLE
Typing the following at the command line:
.LP
.RS
\fBr.to.gnuplot elevation > elev.dat \fR
.RE
.LP
will write the raster data to \fIelev.dat\fR.
After staring the GRASS graphics monitor, the following
dialogue:
.LP
.RS
\fBg.gnuplot\fR
.br
gnuplot> \fBset parametric\fR
.br
gnuplot> \fBset contour base\fR
.br
gnuplot> \fBset nosurface\fR
.br
gnuplot> \fBset view 180,0\fR
.br
gnuplot> \fBsplot 'elev.dat' notitle with lines\fR
.RE
.LP
will plot a contour map of \fIelevation\fR.
.SH NOTES
Similar procedures may be used to plot
wire-mesh surfaces.
.LP
.I g.gnuplot
may be used to simultaneously plot surfaces and contours from
multiple raster maps.
.LP
Output may be saved as 
PostScript, FrameMaker, TeX, etc (approximately 2 dozen output
formats).
.SH FILES
$GISBASE/scripts/r.to.gnuplot.
.SH SEE ALSO
.I r.stats,
.I v.to.gnuplot,
and
.I g.gnuplot
.SH AUTHOR
James Darrell McCauley, Agricultural Engineering, Purdue University
