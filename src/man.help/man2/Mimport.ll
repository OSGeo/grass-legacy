.\" %W% %G%
\."
.\" run me thru tbl first - do make tbl
\."
.TH Mimport.ll 2M GRASSmapdev
.SH NAME
Mimport.ll  \- Creates a UTM-referenced 
.CT "raster file" "cell map"
from lat/long data
.br
.I (Map Development Tool)
.SH SYNOPSIS
.IP Mimport.ll 12 
if\*=\fIinfile\fR of\*=\fIoutfile \fRrows\*=\fI# \fRcols\*=\fI# \fRbpc\*=\fI#\fR[u] latres\*=\fI# \fRlonres\*=\fI# xx\*=lat,lon \fRs\*=\fIspheroid
.R
.SH DESCRIPTION
.I Mimport.ll 
converts data referenced using latitude and longitude coordinates to GRASS
.CT raster "cell map"
format.
.I Mimport.ll 
is primarily used as the final program in converting DTED and DEM data
to GRASS 
.CT raster "cell map"
form, but is not limited to this use.
.I Mimport.ll
uses the user's current 
.CT "window." "geographic window."
Only data that falls in the current 
.CT "window" "geographic window"
will
appear in the final cell 
.CT "file." "map."
.sp
.I Mimport.ll
requires the user to enter nine inputs:
.IP if
The full pathname of a data file referenced using latitude-longitude values.
.IP of
The name of the resultant cell 
.CT "file." "map."
.IP rows
The number of rows of data in the input file.
.IP cols
The number of columns of data in the input file.
.IP bpc
The number of bytes per column in the input file.
The default is 1, if not specified.
If the data is unsigned, append a \fBu\fR to the number.
.IP latres
The latitude resolution of the data in arc-seconds.
.IP lonres
The longitude resolution of the data in arc-seconds.
.IP xx
The latitude and longitude of any one corner of the input file.
xx specifies which corner and should NOT be specified as xx,
but as one of: \fBsw se nw ne\fR
.IP
The latitude and longitude are specified as: dd.mm.ssh
.br
where dd is degrees, mm is minutes, ss is seconds, and h is the hemisphere
(\fBn\fR or \fBs\fR for latitudes, \fBe\fR or \fBw\fR for longitudes).
.IP
For example,  to specify the southwest corner: sw=46.00.00n,120.00.00w
.IP
Note: the latitude and longitude specified are for the center of the cell
at the \fIxx\fR corner.
.IP s
Specifies the name of the spheroid of the local datum
to be used for converting from lat/lon to UTM coordinates.
The following spheroids can currently be specified:

.in +3
.TS
c c c
c c c
l l l.
spheroid	semi-major	eccentricity
name	axis	squared

australian	6378160.0	0.0066945419
bessel	6377739.155	0.0066743722
clark66	6378206.4	0.006768658
clark80	6378249.145	0.0068035113
everest	6377276.345	0.0066378466
international	6378388.0	0.00672267
wgs72	6378135.0	0.006694317778
.TE
.in -3

If your 
.CT database "data base"
is registered to a spheroid which is not in this list,
you may specify the spheroid parameters as follows (no spaces allowed):

.in +3
s\*=a\*=\fIsemi-major-axis\fR,e\*=\fIeccentricity-squared\fR
.in -3
.SH EXAMPLE
The command line:
.br
.IP "Mimport.ll" 11
if=rot.out of=import.out  rows=358  cols=301  bpc=2 latres=3 lonres=3
sw=37.13.00n,103.45.00w  s=wgs72
.LP
.sp
reads data from the file "rot.out", converts the data, and
stores it in the file "import.out". The data to be converted
is made up of 358 rows and 301 columns,
and has a resolution of 3x3 arc seconds.
.SH NOTES
In the conversion of DTED and DEM elevation data to 
.CT raster "cell map"
form, 
.I Mimport.ll
follows execution of the data rotation program 
.I Mrot90.
Because the user can glean information on the number of rows and columns,
the resolutions of the latitude and longitude, and the number of bytes
per column from the header file produced by the tape extraction programs
.I Mdted.extract
and
.I MdmaUSGSread,
the user should recall that
.I Mrot90
has rotated the files produced by the tape extraction programs 90 degrees;
this means that the user should INTERCHANGE the numbers of rows and
columns present in the header file for input to
.I Mimport.ll.
The number of rows shown in the tape extract header file now become
the number of columns in the \fIMrot90\fR output file;  the number of columns
shown in the tape extract header file are now the number of rows present
in the \fIMrot90\fR output file.
.PP
The user should also note that the cell 
.CT file map
imported into GRASS will
be based on the current 
.CT "window." "geographic window."
  The boundaries of this 
.CT "window" "geographic window"
should
therefore be checked before importing the cell 
.CT "file." "map." 
Data outside of
the 
.CT "window" "geographic window"
will not be imported and missing data will become "no data".

.SH "SEE ALSO"
\fIMdmaUSGSread[2M], Mdted.examine[2M], Mdted.extract[2M], Mrot90[2M]\fR
.SH "AUTHOR"
Michael Shapiro, U.S. Army Construction Engineering Research Laboratory
