.TH i.in.pri 1 "" "" "" ""





.SH NAME

\*Li.in.pri\*O  - ERS1.SAR.PRI file to GRASS conversion. 

.br

(GRASS Imagery Program) 

.SH SYNOPSIS

\*Li.in.pri\*O

.SH DESCRIPTION
i.in.pri on the command line.  The program will then prompt the user for parameter values.

.SH Parameters:


.VL 4m

.LI "\*Linput file\*O
Path location and name of file to import.

.LI "\*Loutput files\*O
raw band cell file


.SH EXAMPLE
\*C
.DS
OPTION:   Data information only (y/n) [n]
     key: infonly
 default: n
required: NO
 options: y,n,

OPTION:   Volume directory filename
     key: voldirname
required: YES

OPTION:   Leader filename
     key: leadername
required: YES

OPTION:   Data filename
     key: dataname
required: YES

OPTION:   Name for resultant raster map
     key: output
required: YES
.DE
\*O

.SH NOTES
This program was derived from \*Li.in.gtc\*O.

.SH AUTHOR
Olaf Hellwich, TUM, February 1994
Technische Universitaet Muenchen, Germany

