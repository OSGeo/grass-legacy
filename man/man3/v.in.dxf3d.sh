.TH v.in.dxf3d.sh
.SH NAME
Iv.in.dxf3d.sh R \- Imports contour levels and master contour levels in DXF
file format to GRASS vector file format.
.br
.I "(GRASS Vector Program)"
.SH SYNOPSIS
\fBv.in.dxf3d.sh\fR \fBdxf=\fIname\fR \fBlines=\fIname,name\fR
.SH DESCRIPTION
The v.in.dxf3d.sh data conversion program generates GRASS vector files from a
DXF file with contour levels and master contour levels layers with Z values.

This shell run successively the programs v.in.dxf, v.in.dxf3d and v.support
for both specified layers.


.SH "COMMAND LINE OPTIONS"
.LP
Parameters
.IP \fIdxf\fR
Name of the DXF input design file to be converted to GRASS vector format.
.IP \fIlines\fR
Name(s) of layer(s) in DXF input file containing  the contour levels and
master contour levels with Z values, and the mane(s) to be assigned to the 
GRASS vector files output.

.SH "SEE ALSO"
v.in.dxf, v.in.dxf3d, v.support, v.digit

.SH AUTHOR
The shell was writted by Evaristo Quiroga, Environmental and Territorial 
Analisis Center, UAB (12/95).