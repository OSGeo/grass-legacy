.TH tig.rim.sh
.SH NAME
\fItig.rim.sh\fR \- Generates various vector maps from a rim/TIGER data base.
.br
.I "(GRASS Shell Script)"
.SH SYNOPSIS
\fBtig.rim.sh help\fR
.br
\fBtig.rim.sh\fR \fIdbname tractN1 tractN2 ...\fR
.SH DESCRIPTION
\fItig.rim.sh\fR is a shell script which queries information from a rim
data base using the GRASS command \fIv.db.rim\fR, which is an interface
between GRASS and RIM.  The \fIdbname\fR given on the command line 
should be the name of a rim data base created using \fIv.in.tiger\fR.  
\fItig.rim.sh\fR will create several new vector files.  Three of these
are: a county outline map, a map of tract boundaries within the
county, and a map showing block group boundaries.  For every tract
number given on the input line, additional vector maps will be created
showing: the tract outline, a block group boundary map for each block
group within the tract, and a map showing block boundaries within each
block group.  Output files will be named \fIdbname.county\fR, 
\fIdbname.tract\fR and \fIdbname.bg\fR for the map layers showing the 
county outline, the tract outlines within the county, and the block 
group boundaries within the county.  The vector file showing the boundary 
for an individual tract will be named \fITtractN\fR, where \fItractN\fR 
is the tract number given on the command line.  The vector files created 
to show an individual block group boundary and block boundaries within
that block group will be named using the appropriate tract number and
block group number as part of the name, with suffixes of .bg and .bk
respectively.
.LP
\fBParameters:\fR
.IP \fIdbname\fR 18
Name of an existing rim data base.
.IP \fItractN\fR
Number of a tract located within this county.
.SH NOTES
This command must be installed separately as part of the package of 
routines dealing with the import of Census (TIGER) data.  It requires 
the use of \fIrim\fR and \fIv.db.rim\fR, which must be compiled first.

You must include at least one tract number on the command line for this
command to function.  Use \fItiger.info.sh\fR to obtain all tract 
numbers for a given TIGER type1 data file.

If the master binary vector file created using \fIv.in.tiger\fR is
modified after it is in GRASS, this program will probably not work.
In that situation, the processes from this shell script may simply
be run by hand, using \fIv.db.rim\fR directly, but searching the 
old vector file to find lines for the new vector files without
using the binary offset field (vectoff).

Vector files showing the block boundaries within a block group may
contain hydrology lines, which in fact define the edge of a census block.
.SH "SEE ALSO"
\fIv.in.tig.rim, v.db.rim, tiger.info.sh\fR
.SH AUTHOR
Jim Hinthorne and David Satnik,
GIS Lab, Central Washington University, Ellensburg, WA.
