.THv.sdts.meta.cp
.SH NAME
\fIv.sdts.meta.cp\fR \ - installs supplementary metadata file preparatory 
to creation of an SDTS export dataset.
.br
\fI(GRASS Vector Data Export/Processing Program)\fR
.br
.SH SYNOPSIS
\fBv.sdts.meta.cp\fR
.br
\fBv.sdts.meta.cp help\fR
.br
\fBv.sdts.meta.cp\fR [\fB-f\fR] \fBmetafile\fR=\fIname\fR \fBmap\fR=\fBname\fR
.br
.SH DESCRIPTION
The program provides assistance for the preparation of supplemental metadata
for an SDTS export dataset.  The user specifies a map layer in his current
mapset, and then a file of metadata information pertaining to the named
map.  The program copies the specified metadata file to a standard location
(in the user's mapset, under the \fIdig_misc\fR directory).  Later, when 
\fIv.out.sdts\fR is run for the same map layer, the items in the metadata 
file will be incorporated in various places in the export dataset.
.LP
While the metadata file can be prepared by the user, it's format and
contents are strictly defined.  An alternative is to use the interface
program, \fIv.sdts.meta\fR, with which the user can prepare and install a
correctly formatted metadata file.
.br
.SH "COMMAND LINE OPTIONS"
.LP
\fBFlags:\fR
.IP \fB-f\fR 16
force overwriting of pre-exisiting metadata file.
.LP
\fBParameters:\fR
.IP \fBmap\*=\fIname\fR 16
name of vector map layer to which the specified metadata file applies.
.IP \fBmetafile\*=\fIname\fR 16
name of file to be installed as a metadata file for the specified map.
.LP
.SH NOTES
.LP
The format of the metadata source file is rather highly specified. Following 
is a list of the items that can be included in the file. Note that each item
is preceded by a particular code. The code, and the following ':' must
be entered intact for each included metadata item.  The colon is then
immediately followed by the user-supplied information: 
.sp
.br
IDEN_MPDT:(creation date of original source map; YYYY or YYMMDD format)
.br
IDEN_TITL:(general title for contents of transfer, for TITL field in
IDEN module. If not specified, vector header "map_name" will be
used)
.br
IDEN_COMT:(general comment on transfer, for IDEN module's COMT field)
.br
XREF_HDAT:(name of geodetic datum to which export data are referenced, for
HDAT fieid in XREF module.)
.br
DDSH_ENT_NAME:(for Dictionary/Schema module; name of kind of entity that
\fIdig_att\fR and \fIdig_cats\fR values represent. If not specified, map name will be used.)
.br
DDDF_GRASS_ENT:(definition for entity in "DDSH_ENT_NAME", for Dictionary/
Definition module. if not supplied, simple default is used.)
.br
DDDF_ATTR_NUM:(definition for \fIdig_att\fR values, for Dictionary/Definition
module; if not specified, simple default is used.)
.br
DDDF_ATTR_LABEL:(definition for \fIdig_cats\fR values, for Dictionary/Definition
module; if not specified, simple default is used.)
.br
As noted above, the metadata file can be prepared, and installed, without 
the user having to worry about format details, with \fIv.sdts.meta\fR.
.sp
.SH "SEE ALSO"
.LP
The GRASS-SDTS User Guide
.br
\fIv.sdts.meta, v.sdts.dq.cp, v.out.sdts, v.in.sdts\fR.
.br
.SH AUTHORS 
.LP
David Stigberg, U.S. Army Construction Engineering Research Laboratory
.br
Tin Qian, University of Illinois

