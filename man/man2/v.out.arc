.\" 92/12/18 %J%
.TH v.out.arc
.SH NAME
\fIv.out.arc\fR \- Converts GRASS vector files to ARC/INFO's "Generate" file format.
.br
.I (GRASS Vector Data Export Program)
.SH SYNOPSIS
\fBv.out.arc \fR
.br
\fBv.out.arc help \fR
.br
\fBv.out.arc type\*=\fIname \fBvect\*=\fIname \fBarc_prefix\*=\fIname\fR
.SH DESCRIPTION
\fIv.out.arc\fR
is a GRASS data export program that converts files in GRASS vector format
to ARC/INFO's "Generate" file format.  The companion program \fIv.in.arc\fR
imports data in ARC/INFO's "Generate" format and
converts them to GRASS vector format.

This program can be run either non-interactively or interactively.
The program will be run non-interactively if the user specifies parameter values
on the command line using the following format:
.LP
.RS
\fBv.out.arc type\*=\fIname \fBvect\*=\fIname \fBarc_prefix\*=\fIname\fR
.RE
.LP
Alternately, the user can simply type:
.LP
.RS
\fBv.out.arc\fR
.RE
.LP
on the command line;  in this case, the program will prompt the user for
parameter values.

.LP
\fBParameters:\fR
.IP \fBtype\*=\fIname\fR 18
Coverage (feature) type.
.br
Options:  polygon, line
.IP \fBvect\*=\fIname\fR 18
The name of a GRASS vector file to be converted to ARC/INFO format.
.IP \fBarc_prefix\*=\fIname\fR 18
A prefix to be assigned to the ARC/INFO-format files output by \fIv.out.arc\fR.


.SH INTERACTIVE MODE:  USER PROMPTS
\fIv.out.arc\fR will prompt
the user to enter the name of a GRASS vector file to be exported to 
ARC/INFO and for a filename prefix to be used in naming the files
created by the program.

A GRASS vector file to be exported to ARC/INFO must either
contain only linear features (i.e., have only line coverage)
or contain only area edge features (i.e., have only polygon coverage).
.I v.out.arc
will begin by asking the user which type of coverage (line or polygon)
is to be imported:

.nf
      COVERAGE TYPE
     Enter "polygon" or "line"
     Hit RETURN to cancel request
     >
.fi
 
The program then prompts the user for the name of the GRASS vector file to be
converted to ARC/INFO format:

.nf
      VECTOR (DIGIT) FILENAME 
     Enter 'list' for a list of existing binary vector files
     Hit RETURN to cancel request
     >
.fi

Next, the user is asked for a file-name prefix to be used in naming
the output ARC/INFO Generate format files:

.nf
      ARC/INFO (GENERATE) FILENAME PREFIX
     Hit RETURN to cancel request
     >
.fi

The filename prefix will be used to name the various files
that will be created for export to ARC/INFO.
When labeled polygon coverage data are exported,
three such files will be created:  a \fIlines file\fR with the suffix .lin,
a \fIlabel-points file\fR with the suffix .lab, and a 
\fIlabel-text file\fR with the suffix .txt.
When line coverage data are exported, two such files will be created:
a \fIlines file\fR with the suffix .lin, and a \fIlabel-text file\fR
with the suffix .txt.
Export of unlabeled polygon or line coverage data will result in creation
of a \fIlines file\fR (.lin suffix) only.
See the DATA FILE FORMATS section for more information on these files.


.SH EXAMPLE
Linear features and polygon data are made up of the series of arcs
(aka, vectors) outlining their perimeters.
The \fIARC/INFO Users' Guide\fR, in its discussion of the \fIUngenerate\fR
command, explains how line and polygon coverage data can be created from files
(like \fIprefix.lin\fR and \fIprefix.pol\fR)
containing the geographic coordinates of these arcs,
and from files (like \fIprefix.lab\fR)
containing the geographic coordinates of label-points.
Below is an example which illustrates
the creation, within ARC/INFO, of a polygon coverage data file
(named \fIsoils\fR) from the files \fIsoils.pol\fR and \fIsoils.lab\fR.

.nf
   Arc: GENERATE SOILS 
   Generate: INPUT soils.pol
   Generate: LINES 
   Generating lines ...
   Generate: INPUT soils.lab
   Generate: POINTS
   Generating points ...
   Generate: QUIT
   Arc: _
.fi

The above example would create a polygon coverage data file named \fIsoils\fR
with label-points. 
The label-points would have ID numbers that correspond to the GRASS
category values for the polygons in the coverage.  The INFO portion 
of ARC/INFO can be used to associate these label-point ID numbers 
with descriptive text from the \fIsoils.txt\fR file.

.SH DATA FILE FORMATS
LINES FILE, also know as \fIprefix.lin\fR or \fIprefix.pol\fR file:
.br
This text file is a "Generate" format lines file.  The \fIlines\fR option of the 
ARC/INFO \fIGenerate\fR command can be used to read this file into ARC/INFO.  
Each line in the file has a unique line-ID number.

.nf
.in +5
101
223343.62 218923.15
223343.62 222271.06
259565.31 222271.06
259565.31 195577.37
END
102
237862.53 203392.37
244970.75 203744.28
253137.66 195577.37
259565.31 195577.37
END
103
237862.53 203392.37
237862.53 203744.28
223343.62 218392.37
END
104
239072.44 186200.56
237862.53 187410.50
237862.53 203392.37
END
END
.in -5
.fi

LABEL-POINTS FILE, also known as \fIprefix.lab\fR file:
.br
This text file will be created by
.I v.out.arc
if the vector file being exported represents a polygon coverage.
\fIprefix.lab\fR consists of a list of label-point (x,y) coordinates,
each with a unique label-point ID number.  

.nf
.in +5
1 242777.81 211533.09
2 243458.37 199282.28
3 243458.37 195199.28
.in -5
.fi

LABEL-TEXT FILE, also known as \fIprefix.txt\fR file:
.br
In the case of polygon coverage data, this file associates an integer
category value
and a category label ("attribute") text string (containing no spaces)
with each label-point ID number.
In the case of line coverage data, this file associates an integer category value
and an attribute text string with each line-ID number.

The first column is the row number (which is arbitrary), the second column 
contains the category value, the third column holds the line or label-point ID
number, and the fourth column contains the attribute text string.

.nf
.in +5
1 4 1 Coniferous
2 5 2 Deciduous
3 2 3 Rangeland
.in -5
.fi
.SH SEE ALSO
.I v.in.arc,
.I v.support
.SH AUTHOR
.nf 
Dave Johnson
DBA Systems, Inc.
10560 Arrowhead Drive
Fairfax, Virginia 22030
