.TH v.to.gnuplot
.SH NAME
\fIv.to.gnuplot\fR \- outputs an ASCII vector map in GNUPLOT format
.br
.I "(GRASS Shell Script)"
.SH SYNOPSIS
\fBv.to.gnuplot help\fR
.br
\fBv.to.gnuplot\fR \fIname\fR
.SH DESCRIPTION
.I v.to.gnuplot
is an \fIawk\fR shell script that converts an ASCII vector map into
a format suitable for plotting with
.I g.gnuplot
and writes the results to standard output.
.SH OPTIONS
This program runs non-interactively;  the user must either
state all parameter values on the command line or use redirection.
.LP
\fBParameter:\fR
.IP \fIname\fR 18
Full path of an ASCII vector  map layer.
.SH EXAMPLE
Typing the following at the command line:
.LP
.RS
\fBv.to.gnuplot < LOCATION/dig_ascii/elevation > elev.dat \fR
.RE
.LP
will write the raster data to \fIelev.dat\fR.
After staring the GRASS graphics monitor, the following
dialogue:
.LP
.RS
\fBg.gnuplot\fR
.br
gnuplot> \fBplot 'elev.dat' notitle with lines\fR
.RE
.LP
will plot a map of \fIelevation\fR.
.SH NOTES
Output may be saved as 
PostScript, FrameMaker, TeX, etc (approximately 2 dozen output
formats).
.LP
\fIv.vclean\fR and \fIv.out.ascii\fR must
be run prior to \fIv.to.gnuplot\fR.
.SH FILES
$GISBASE/scripts/v.to.gnuplot
.SH SEE ALSO
.I v.clean,
.I v.out.ascii,
.I r.to.gnuplot,
and
.I g.gnuplot
.SH AUTHOR
James Darrell McCauley, Agricultural Engineering, Purdue University
