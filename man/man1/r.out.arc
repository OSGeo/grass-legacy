.TH r.out.arc 1 "" "" "" ""
.SH NAME
\*Lr.out.arc\*O  - Converts a raster map layer 
into an ESRI ARCGRID file.
.br
(GRASS Raster Data Export Program)
.SH SYNOPSIS
\*Lr.out.arc\*O
.br
\*Lr.out.arc help\*O
.br
\*Lr.out.arc \*O[\*L-h\*O]  \*O[\*L-1\*O] \*Lmap=\*Oname
[\*Ldp=\*Ovalue]
.SH DESCRIPTION
r.out.arc converts a user-specified raster map
layer (\*Lmap=\*Oname) into an ESRI ARCGRID ascii file
suitable for export to other computer systems.  The
dg=value is a
number of the user's choice) can be used to request that
numbers after decimal points are limited.   However, to
use this, the user should know the maximum number of digits
that will occur in the output file.  The user
can find the maximum number of digits occurring in the
output file by running r.out.arc without the
\*Ldg=\*Ovalue option.
.PP
The GRASS program 
\*Lr.in.arc\*O 
can be used to perform the reverse
function, converting an ESRI ARCGRID ascii file in suitable format to GRASS
raster file format. The order of cell values in file is from lower left to
upper right (reverse to GRASS).
.SH OPTIONS
.SH Flags:
.VL 4m
.LI "\*L-h\*O 
Suppress printing of header information.
.LE
.VL 4m
.LI "\*L-1\*O 
List one entry per line.
.LE
.SH Parameters:
.VL 4m
.LI "\*Lmap=\*Oname 
Name of an existing raster map layer.
.LI "\*Ldg=\*Ovalue 
The minimum number of decimals (per cell) to be printed.
.LE
r.out.arc can be run either non-interactively or
interactively.  The program will be run non-interactively
if the user specifies the name of a raster map layer and
(optionally) a value for dg, using the form
.VL 4m
\*Lr.out.arc map=\*Oname ]
.LE
where name is the name of a raster map layer to be
converted to ARCGRID format, and value is the
minimum number of digits (per cell) to be printed to
output.  The user can also the \*L-h\*O option to suppress
the output of file header information.
.PP
Alternately, the user can simply type \*Lr.out.arc\*O on the command line,
without program arguments.  In this case, the user will be prompted for
parameter values using the standard GRASS 
\*Lparser\*O interface.
.SH NOTES
The output from r.out.arc may be placed into a file
by using the UNIX redirection mechanism;  e.g.:
.VL 4m
\*Lr.out.arc map=\*Osoils > out.grd
.LE
The output file out.grd can then be copied
onto a magnetic tape or floppy disk for export purposes.
.SH SEE ALSO
\*Lr.in.arc\*O
.br
\*Lparser\*O
.SH AUTHOR
Markus Neteler, University of Hannover, Germany, based on r.out.ascii from 
.br

Michael Shapiro,
U.S.Army Construction Engineering 
Research Laboratory
