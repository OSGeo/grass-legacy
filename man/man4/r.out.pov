.TH r.out.pov
.SH NAME
\fIr.out.pov\fR \- Converts a raster map layer into a height-field file for POVRAY.
.br
.I (GRASS Raster Data Export Program)
.SH SYNOPSIS
\fBr.out.pov\fR
.br
\fBr.out.pov help\fR
.br
\fBr.out.pov \fR[\fB-h\fR] \fBmap=\*=\fIname \fBtga=\*=\fIname \fR[\fBhftype=\*=\fIvalue\fR] \fR[\fBbias=\*=\fIvalue\fR] \fR[\fBscale=\*=\fIvalue\fR] 
.SH DESCRIPTION
.I r.out.pov
converts a user-specified raster map layer (map=\*=\fIname\fR)
into a height-field file for POVray (tga=\*=\fIname\fR).
The hftype=\*=\fIvalue\fR option (where \fIvalue\fR is either 0 or 1)
specifies the height-field type. When the user enters 0 the output
will be actual heights. If entered 1 the cell-values will be
normalized. If \fIhftype\fR is 0 (actual heights) the bias=\*=\fIvalue\fR
can be used to add or substract a value from heights.
Use scale=\*=\fIvalue\fR to scale your heights by \fIvalue\fR.
The GRASS program \fIr.out.pov\fR can be used to create height-field
files for Persistence of Vision (POV) raytracer. POV can use a 
height-field defined in Targa (.TGA) image file format where the
RGB pixel values are 24 bits (3 bytes). A 16 bit unsigned integer
height-field value is assigned as follows: RED = high byte, GREEN =
low byte, BLUE = empty.

\fBParameters:\fR
.IP \fBmap=\*=\fIname\fR 18
Name of an existing raster map layer.
.IP \fBtga=\*=\fIname\fR 18
Name of TARGA outputfile (one should add the extension .tga).
.IP \fBhftype=\*=\fIvalue\fR 18
0=actual heights, 1=normalized heights.
.IP \fBbias=\*=\fIvalue\fR 18
Bias which is added or substracted to heights.
.IP \fBscale=\*=\fIvalue\fR 18
Value to stretch or shrink elevations.
.LP
.LP
\fIr.out.pov\fR can be run either non-interactively or interactively.
The program will be run non-interactively if the user specifies
the name of a raster map layer and a name for \fItga\fR (output),
using the form
.LP
.RS
\fBr.out.pov map=\*=\fIinname\fB tga=\*=\fIoutname 
.RE
.LP
where \fIinname\fR is the name of a raster map layer to
be converted to POV format, and \fIoutname\fR is the name
of the outputfile. Further optional values can be entered.
.LP
Alternately, the user can simply type \fBr.out.pov\fR on the command line,
without program arguments.  In this case, the user will be prompted for
parameter values using the standard GRASS parser interface
described in the manual entry for \fIparser\fR.
.LP
.RS
\fBr.out.pov map=\*=elevation tga=\*=out.tga\fR
.RE
.SH AUTHOR
Klaus Meyer, GEUM.tec GbR, eMail: GEUM.tec@geum.de
