.TH r.in.arc 1 "" "" "" ""
.SH 
NAME
\*L\*Wr.in.arc\*O\*O - Convert an ESRI ARC/INFO ascii raster file (GRID)
into a (binary) raster map layer.

.br
\*W(GRASS Raster Data Import Program)\*O
.SH 
SYNOPSIS
\*Lr.in.arc\*O

.br
\*Lr.in.arc help\*O

.br
\*Lr.in.arc input=\*O\*Wname\*O \*Loutput=\*O\*Wname\*O [\*Ltitle=\*O"\*Wphrase\*O"]
[\*Lmult=\*O\*Wmultiplier\*O]
.SH 
DESCRIPTION
\*Wr.in.arc\*O allows a user to create a (binary) GRASS raster map layer
from an ESRI ARC/INFO ascii GRID file with (optional) title.
.SH 
OPTIONS
.SH 
Parameters:
.VL 4m
.LI "\*Linput=\*O\*Wname\*O"
Name of an existing ASCII raster file to be imported.
.LI "\*Loutput=\*O\*Wname\*O"
Name to be assigned to resultant binary raster map layer.
.LI "\*Ltitle=\*O"\*Wphrase\*O""
Title to be assigned to resultant raster map layer.
.LI "\*Lmult=\*O\*Wmultiplier\*O"
Multiply all raster cell values by \*Wmultiplier\*O. \*Wmultiplier\*O
is a floating point value, and has a default value of 1.0.
.LE
The \*Linput\*O file has a header section which describes the location
and size of the data, followed by the data itself.
The header has 6 lines:
\*C
.DSncols:
nrows:
xllcorner:
yllcorner:
cellsize:
or alternatively (not supported in r.in.arc):
\*C
.DSncols:
nrows:
xllcenter:
yllcenter:
cellsize:

.SH 
NOTES
...
<p>\*Wr.in.arc\*O handles floating point cell values. The \*Lmult\*O
option allows the number of significant figures of a floating point cell
to be increased before importing. Multiples of ten are the most functional
multipliers.
.SH 
SEE ALSO
\*W\*Lr.out.arc\*O\*O
.SH 
AUTHOR
Unknown German author, updated by Bill Brown to floating point support.
