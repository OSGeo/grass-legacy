.TH v.to.sites
.SH NAME
\fIv.to.sites\fR \- Converts point data in a binary GRASS vector map layer into
a GRASS \fIsite_lists\fR file.
.br
.I "(GRASS Vector Program)"
.SH SYNOPSIS
.B v.to.sites
.br
.B v.to.sites help
.br
\fBv.to.sites input\*=\fIname \fBoutput\*=\fIname\fR
.SH DESCRIPTION
The \fIv.to.sites\fR program extracts site data from a GRASS vector map layer
and stores output in a new GRASS \fIsite_lists\fR file.
The resulting sites map layer can be used with such programs as \fId.sites\fR.
Only site (point) features in the named vector map layer are extracted
and placed into the resulting site list.  Lines and areas in the vector
file are ignored.
.LP
The user can run the program non-interactively by specifying the names
of an existing vector \fIinput\fR map layer and a new site list file
to be \fIoutput\fR on the command line.
The program will be run interactively if the user types \fBv.to.sites\fR
without arguments on the command line.  In this case, the user will
be prompted to enter parameter values through the standard user interface
described in the manual entry for \fIparser\fR.

.LP
\fBParameters:\fR
.IP \fBinput\*=\fIname\fR 18
Name of an existing binary vector map layer from which
site data are to be extracted.
.IP \fBoutput\*=\fIname\fR 18
Name to be assigned to the resultant \fIsite_lists\fR file.
.LP
If any of the sites have been labeled in 
.I v.digit,
then the resultant site list will contain category information.
If none of the sites are labeled, a binary (0/1) site list file will be produced.
.SH "SEE ALSO"
.I d.sites,
.I s.db.rim,
.I s.menu,
.I v.db.rim,
.I v.digit
and
.I parser
.SH "AUTHOR"
Dave Gerdes, U.S. Army Construction Engineering Research Laboratory
