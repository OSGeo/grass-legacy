.TH Gsurface.sl 2G G-language
.SH NAME
Gsurface.sl \- Fit a continuous surface to a set of data points stored in
a sites file
.SH SYNOPSIS
\fBGsurface.sl fit=\fItype \fR[\fBgrid=\fIrows\fR[\fBX\fIcols\fR]]
\fBsite=\fIsites_file \fBcell=\fIcell_file\fR
.SH DESCRIPTION
.I Gsurface.sl
fits a continuous surface to data points found in the specified
\fIsites_file\fR.
In addition to specifying the
.I sites_file
containing the data points, the user must specify the
.I cell_file
to receive the calculated surface.  The calculation will produce
a cell file with the user's current window.  The user must also
specify the
.I type
of fit he desires to have performed.  If
.I type
is "spline",
.I rows
and possibly
.I cols
must also be given.  If only
.I rows
is given,
.I cols
is assumed to be the same.
.SH OPTIONS
.IP \fBfit=\fItype\fR
\fItype\fR is the type of algorithm to be used to estimate the surface.
Possible choices are "spline", "d1" and "d2".  Spline will give the
smoothest fit of the three.  D1 and d2 type estimates are rougher.
There is little difference between d1 results and d2 results.

.IP \fBgrid=\fIrows\fBX\fIcols\fR
\fIRows\fR and \fIcols\fR are the number of rows
and columns to be used for an intermediate
calculation when using the spline algorithm.  If
.I cols
is not specified, then it is assumed to be the same as \fIrows\fR.
.I Rows
and
.I cols
are ignored for the d1 and d2 algorithms.  In general, the smaller
.I rows
and \fIcols\fR,
the smoother the surface obtained.  The larger
.I rows
and \fIcols\fR,
the more spline results will resemble d1 and d2 results.  As
a general rule,
.I rows
and
.I cols
should be around one fifth to one tenth the number of rows and columns,
respectively, in the target cell file.

.IP \fBsite=\fIsites_file\fR
specifies the name of a sites file which contains the data points.
The sites file is assumed to be in the current mapset.

.IP \fBcell=\fIcell_file\fR
specifies the name of a cell file which will be produced in the
current mapset with the current window.
.SH AUTHOR
Jean Ezell, U.S. Army Construction Engineering Research Laboratory
