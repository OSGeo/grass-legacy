.THv.sdts.dq.cp
.SH NAME
\fIv.sdts.dq.cp\fR \ - installs SDTS data quality reports.
.br
\fI(GRASS Vector Data Export/Processing Program)\fR
.br
.SH SYNOPSIS
\fBv.sdts.dq.cp\fR
.br
\fBv.sdts.dq.cp help\fR
.br
\fBv.sdts.dq.cp\fR [\fB-f\fR] \fBmap\*=\fIname\fR [\fBH\*=\fIname\fR] [\fBPA\*=\fIname\fR] [\fBAA\*=\fIname\fR] [\fBLC\*=\fIname\fR] [\fBCG\*=\fIname\fR]
.br
.SH DESCRIPTION
The program provides assistance for the preparation of the five data quality
report modules (Lineage, Positional Accuracy, Attribute Accuracy, Logical
Consistency, and Completeness) required in an SDTS transfer dataset.
The program has one simple function:  the user specifies a map layer in
his current mapset, and then one or more files to be used for SDTS data 
quality reports for this map layer.  The program copies the specified files 
to a standard location (in the user's current mapset, under the \fIdig_misc\fR
directory).  Later, when \fIv.out.sdts\fR is run for the same map layer, the 
data quality reports will be incorporated into an SDTS export dataset.
.sp
.SH "COMMAND LINE OPTIONS"
.LP
.\fBFlags:\fR
.IP \fB-f\fR 16
force overwriting of pre-exisiting data quality report(s).
.LP
\fBParameters:\fR
.IP \fBmap\*=\fIname\fR 16
name of vector map layer to which the specified data quality report files
apply.
.br
\fBHL\*=\fIname\fR
name of file to be used for the SDTS Lineage (HL) data quality report.
.br
\fBPA\*=\fIname\fR
name of file to be used for the SDTS Positional Accuracy (PA) data quality 
report.
.br
\fBAA\*=\fIname\fR
name of file to be used for the SDTS Attribute Accuracy (AA) data quality 
report.
.br
\fBLC\*=\fIname\fR
name of file to be used for the SDTS Logical Consistency (LC) data quality 
report.
.br
\fBCG\*=\fIname\fR
name of file to be used for the SDTS Completeness (CG) data quality report.
.br
.SP
.SH NOTES
.L
Data Quality report files should be simple narrative text files.  After the
files have been installed with \fIv.sdts.dq.cp\fR, \fIv.out.sdts\fR will 
convert the
installed copy of each report to SDTS ISO 8211 format.  Each paragraph in
the original file will become a separate record in the SDTS data quality 
module.
.LP
Parameter names--HL, PA, AA, LC, CG--are SDTS codes for different
data quality modules (HL=Lineage, PA=Positional Accuracy, etc.).
.LP
Data quality files to be installed can be created as well as installed
with \fIv.sdts.meta\fR.
.sp
.SH "SEE ALSO"
.LP
GRASS-SDTS User Guide
.br
\fIv.sdts.meta, v.sdts.meta.cp, v.out.sdts, v.in.sdts\fR.
.br
.SH AUTHORS 
.LP
David Stigberg, U.S. Army Construction Engineering Research Laboratory
.br
Tin Qian, University of Illinois

