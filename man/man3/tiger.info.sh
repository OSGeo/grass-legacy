.TH tiger.info.sh
.SH NAME
\fItiger.info.sh\fR \- Provides tract number(s) and classification codes found within a given U.S. Census Bureau TIGER type1 data file.
.br
.I "(GRASS Shell Script)"
.SH SYNOPSIS
\fBtiger.info.sh help\fR
.br
\fBtiger.info.sh \fIinfile\fR
.SH DESCRIPTION
\fItiger.info.sh\fR is a shell script which outputs tract number(s) and
classification codes found within the TIGER type1 data file \fIinfile\fR.
Output is written to standard out, and can be captured in a file by 
redirecting output.  This information is useful when querying (using
\fIv.db.rim\fR) from the master binary vector file created by 
\fIv.in.tig.rim\fR.  It also provides tract number(s) which can
be used as input to the command \fIGen.Maps\fR. 
.LP
\fBParameters:\fR
.IP \fIinfile\fR 18
Name of a TIGER type1 data file.
.SH NOTES
This command must be installed separately as part of the package of routines
dealing with the import of Census (TIGER) data. 
.SH "SEE ALSO"
\fIm.tiger.region, v.in.tig.rim, v.db.rim, Gen.Maps, Gen.tractmap\fR
.SH AUTHOR
Marjorie Larson, U.S. Army Construction Engineering Research Laboratory
