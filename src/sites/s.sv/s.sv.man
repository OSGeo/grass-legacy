.TH s.sv
.SH NAME
\fIs.sv\fR \- Sample semivariogram of a GRASS sites list.
.br
.I (GRASS Sites Program)
.SH SYNOPSIS
\fBs.sv\fR
.br
\fBs.sv help\fR
.br
\fBs.sv\fR [\fB-alq\fR] 
[\fBsites\*=\fIname\fR]
\fBlag\*=\fIvalue\fR 
[\fBlagtol\*=\fIvalue\fR] 
[\fBdirection\*=\fIvalue\fR] 
[\fBangtol\*=\fIvalue\fR] 
[\fBgraph\*=\fIname\fR] 
.SH DESCRIPTION
\fIs.sv\fR calculates a sample semivariogram and either plots
it or writes it to standard output.
.LP
For more information, refer to the tutorial or see
the example below.
.SH OPTIONS
\fBFlags:\fR
.IP \fB-q\fR 18
Quiet. Cut out the chatter.
.IP \fB-p\fR 18
Plot the sample semivariogram in the GRASS graphics window
(requires \fIg.gnuplot\fR).
.LP
\fBParameters:\fR
.IP \fBsites\*=\fIname\fR 18
Name of an existing sites file with floating-point
attributes (the variable for which we are calculating semivariance). 
Default is standard input with no field separators. 
.IP \fBlag\*=\fIvalue\fR 18
Nominal lag distance.
.IP \fBlagtol\*=\fIvalue\fR 18
Tolerance on lag distance. Default is half of nominal distance.
.IP \fBdirection\*=\fIvalue\fR 18
Direction of semivariogram. Default is omnidirectional semivariogram.
.IP \fBangtol\*=\fIvalue\fR 18
Angular tolerance on direction.
.IP \fBgraph\*=\fIname\fR 18
Basename to save graphing data/commands files.
Graphs are saved in the current working directory with
the extensions \fI.gp\fR and \fI.dat\fR. Implies
the \fB-p\fR flag. If unspecified, semivariogram is
written to standard output.
.LP
.SH NOTES
Without the \fB-p\fR flag, three columns of
data are written to standard output: lag distance (\fIh\fR),
semivariogram value (\fIgamma\fR), and the number of data pairs
used to compute it (\fIN(h)\fR). When the \fBgraph\fR parameter
is set, these same three columns of data are
written to \fIname\fR.dat. Therefore, to replot
a sample semivariogram, use:
.LP
.RS
\fBg.gnuplot \fIname\fR.gp
.RE
.LP
To plot a histogram of \fIN(h)\fR, simply edit \fIname\fR.gp
and redo the previously given command.
.SH SEE ALSO
.I s.univar,
.I s.normal,
.I g.gnuplot,
.I m.svfit 
and
.br
\fISemivariogram Modeling\fR \-
A GRASS Tutorial on Exploratory
Data Analysis and Semivariogram Modeling.
.SH BUGS
Will not work correctly with lat-long data. 
Should \fIG_azimuth()\fR be used to
calculate the angle between points?
.LP
Only Matheron's classical estimator is available with \fIs.sv\fR.
Others may be added in the future.
.LP
Please send all bug fixes and comments to the author.
.SH AUTHOR
James Darrell McCauley, Agricultural Engineering, Purdue University 
.if n .br 
(mccauley@ecn.purdue.edu)
