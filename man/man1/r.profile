.TH r.profile
.SH NAME
\fIr.profile\fR \- Outputs the raster map layer values lying on user-defined line(s).
.br
.I "(GRASS Raster Program)"
.SH SYNOPSIS
\fBr.profile\fR
.br
\fBr.profile help\fR
.br
.nf
\fBr.profile\fR \fBmap\fR\*=\fIname\fR [\fBresult\fR\*=\fItype\fR] \
[\fBwidth\fR\*=\fIvalue\fR]
    \fBline\fR\*=\fIeast,north,east,north\fR[\fI,east,north,east,north,...\fR]
.fi
.SH DESCRIPTION
This program outputs, in ASCII, the values assigned to those cells
in a raster map layer that lie along one or more lines ("profiles").
The lines are described by their starting and ending coordinates.
The profiles may be single-cell wide lines, or multiple-cell wide lines.
The output, for each profile, may be the category values assigned to each
of the cells, or a single aggregate value (e.g., average or median value).
.SH "COMMAND LINE OPTIONS"
\fBParameters:\fR
.IP \fBmap\*=\fIname\fR 18
Raster map to be queried.
.IP \fBresult\*=\fItype\fR 18
Type of result to be output.
.br
Options:  raw, median, average
.br
Default:  raw

Raw results output each of the category values assigned to all cells
along the profile.  Median and average output a single value per profile:
average outputs the average category value of all cells under
the profile;  median outputs the median cell category value.
.IP "\fBline\*=\fIeast,north,east,north\fR[\fI,east,north,east,north,...\fR]" 18
The geographic coordinates of the starting and ending points that define
each profile line, given as easting and northing coordinate pairs.
The user must state the starting and ending coordinates of at least one line,
and may optionally include starting and ending coordinates of additional lines.
.IP \fBwidth\*=\fIvalue\fR
Profile width, in cells (odd number).
.br
Default:  1

Wider profiles can be specified by setting the width to 3, 5, 7, etc.
The profiles are then formed as rectangles 3, 5, 7, etc., cells wide.
.SH OUTPUT FORMAT
The output from this command is printed to the standard output in ASCII.
The format of the output varies slightly depending on the type of result.
The first number printed is the number of cells associated with the profile.
For raw output, this number is followed by the individual cell values.
For average and median output, this number is followed by a single value
(i.e., the average or the median value).

These examples are for the 
\fIelevation.dem\fR raster map layer in the \fIspearfish\fR
sample data set distributed with GRASS 4.0:

Single-cell profile:
.LP
.RS
.B "r.profile map\*=elevation.dem line\*=593655,4917280,593726,4917351"

4 1540 1551 1557 1550
.RE


3-cell wide profile:
.RS
.B "r.profile map\*=elevation.dem line\*=593655,4917280,593726,4917351 width\*=3"

22 1556 1538 1525 1570 1555 1540 1528 1578 1565 1551 1536 1523 1569 1557 1546 1533 \
1559 1550 1542 1552 1543 1548

(Output appears as multiple lines here, but is really one line)
.RE


3-cell wide profile average:
.LP
.RS
.B "r.profile map\*=elevation.dem line\*=593655,4917280,593726,4917351 width\*=3 result\*=average"

22 1548.363636
.RE


3-cell wide profile median:
.LP
.RS
.B "r.profile map\*=elevation.dem line\*=593655,4917280,593726,4917351 width\*=3 result\*=median"

22 1549.000000
.RE
.SH "SEE ALSO"
.I d.profile,
.I r.transect
.SH AUTHOR
Michael Shapiro, U.S. Army Construction Engineering Research Laboratory
