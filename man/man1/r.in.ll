.TH r.in.ll
.SH NAME
\fIr.in.ll\fR \- Converts raster data referenced using latitude and longitude
coordinates to a UTM-referenced map layer in GRASS raster format.
.br
.I "(GRASS Raster Data Import Program)"
.SH SYNOPSIS
\fBr.in.ll\fR
.br
\fBr.in.ll help\fR
.br
.na
.in +3
.ti -3
\fBr.in.ll\fR [\fB\-s\fR] \fBinput\fR\*=\fIname \fBoutput\fR\*=\fIname \fBbpc\fR\*=\fIvalue \fBcorner\fR\*=\fIcorner,lat,lon\fR 
\fBdimension\fR\*=\fIrows,cols \fBres\fR\*=\fIlatres,lonres \fBspheroid\fR\*=\fIname\fR
.ad
.in
.SH DESCRIPTION
This program converts raster data referenced using latitude and longitude
coordinates to a UTM-referenced map layer in GRASS raster format.
.I r.in.ll 
is primarily used as the final program in converting DTED and DEM digital
elevation data to GRASS raster format, but is not limited to this use.
.I r.in.ll
uses the user's current geographic region settings.
Only data that falls within the current geographic region will
appear in the final raster map layer.
.sp
.I r.in.ll
requires the user to enter the following information:

.SH "COMMAND LINE OPTIONS"
.LP
\fBFlags:\fR
.IP \fB-s\fR 18
Signed data (high bit means negative value).

.LP
\fBParameters:\fR
.IP \fBinput\*=\fIname\fR 18
Name of an existing input raster map layer.
.IP \fBoutput\*=\fIname\fR 18
Name to be assigned to the output raster map layer.
.IP \fBbpc\*=\fIvalue\fR 18
Number of bytes per cell.
.IP \fBcorner\*=\fIcorner,lat,lon\fR 18
One corner latitude and longitude of the input.
.br
Format:  {nw\*|ne\*|sw\*|se},dd:mm:ss{N\*|S},ddd:mm:ss{E\*|W}

The latitude and longitude are specified as
.B dd.mm.ssH
where dd is degrees, mm is minutes, ss is seconds, and H is the hemisphere
(\fBN\fR or \fBS\fR for latitudes, \fBE\fR or \fBW\fR for longitudes).

For example, to specify the southwest corner:  corner\*=sw,46N,120W

Note:  the latitude and longitude specified are for the center of the corner cell.
.IP \fBdimension\*=\fIrows,cols\fR 18
Number of rows and columns in the input file.
.IP \fBres\*=\fIlatres,lonres\fR 18
Resolution of the input (in arc seconds).
.IP \fBspheroid\*=\fIname\fR 18
Name of spheroid to be used for coordinate conversion.
.br
Options:  airy, australian, bessel, clark66, everest, grs80, hayford, international,
krasovsky, wgs66, wgs72, wgs84
.SH EXAMPLE
The command line:

.RS
.na
.in +3
.ti -3
\fBr.in.ll input\*=rot.out output\*=import.out dimension\*=358,301 bpc\*=2
res\*=3,3 corner\*=sw,37:13N,103:45W spheroid\*=wgs72 \fR
.in
.ad
.RE

reads data from the file \fIrot.out\fR, converts the data, and
stores them in the file \fIimport.out\fR.  The data to be converted
are made up of 358 rows and 301 columns,
and have a resolution of 3x3 arc seconds.
.SH NOTES
In the conversion of DTED and DEM elevation data to raster map layer format,
.I r.in.ll
follows execution of the data rotation program \fIm.rot90\fR.
Because the user can glean information on the number of rows and columns,
the resolutions of the latitude and longitude, and the number of bytes
per column from the header file produced by the tape extraction programs
\fIm.dted.extract\fR and \fIm.dmaUSGSread\fR, the user should recall that
.I m.rot90
has rotated the files produced by the tape extraction programs 90 degrees;
this means that the user should INTERCHANGE the numbers of rows and
columns present in the header file for input to \fIr.in.ll\fR.
The number of rows shown in the tape extract header file now become
the number of columns in the \fIm.rot90\fR output file;  the number of columns
shown in the tape extract header file are now the number of rows present
in the \fIm.rot90\fR output file.
.LP
The user should also note that the raster map layer imported into GRASS will
be based on the current geographic region settings.
The boundaries of this geographic region should therefore be checked
before importing the raster map layer.
Data outside of the geographic region will not be imported
and missing data will be assigned the category value "no data".
.SH "SEE ALSO"
.I m.dmaUSGSread,
.I m.dted.examine,
.I m.dted.extract,
.I m.rot90
.SH "AUTHOR"
Michael Shapiro, U.S. Army Construction Engineering Research Laboratory
