.TH v.in.arc
.SH NAME
\fIv.in.arc\fR \- Converts data in ARC/INFO format to GRASS's vector format,
and stores output in the user's current GRASS mapset.
.br
\fI(GRASS Vector Data Import Program)\fR
.SH SYNOPSIS
\fBv.in.arc \fR
.br
\fBv.in.arc help \fR
.br
\fBv.in.arc \fR[\fB-n\fR] \fBtype\*=\fIname \fBlines_in\*=\fIname \fR[\fBpoints_in\*=\fIname\fR] [\fBtext_in\*=\fIname\fR]
    \fBvector_out\*=\fIname \fR[\fBidcol\*=\fIvalue\fR] [\fBcatcol\*=\fIvalue\fR] [\fBattcol\*=\fIvalue\fR]
.SH DESCRIPTION
The user may wish to use GRASS programs on data files that were created by other GISs.
To do this, the user must first convert data files in these systems' formats to
GRASS's file format.  Bringing data from other systems into GRASS is termed
file \fIimport\fR.  Sending GRASS data files out into other systems' formats is
termed file \fIexport\fR.

A variety of GRASS programs exist to import and export data to and from GRASS.
The \fIv.in.arc\fR program will convert vector data in ARC/INFO's "Generate" format
to GRASS's vector file format, and bring it into the user's current GRASS mapset.
The files to be imported to GRASS must first have been exported from ARC/INFO
using the ARC/INFO \fIUngenerate\fR command, and may represent either
linear features ("line coverage") or areal features ("polygon coverage").
The \fIARC/INFO User's Guide\fR describes how files containing linear
and polygonal features can be exported from ARC/INFO, in a section
detailing the use of the \fIUngenerate\fR command.

\fBNote:\fR  The ARC coverage must be set to single precision before
running \fIUngenerate\fR.  If it is not, first
copy it to another coverage that is set to single precision, then
run \fIUngenerate\fR.

Once converted with the ARC/INFO \fIUngenerate\fR command, the files to be
imported into GRASS must be placed in a directory named \fIarc\fR in the
user's current mapset. If the \fIarc\fR directory does not exist, it must be
created (e.g., with the command \fImkdir $LOCATION/arc\fR) before copying the 
ARC-INFO files to be converted into it.

(\fIv.in.arc\fR can be used to convert ARC-INFO data from other mapsets as
well, since the program searches for the specified input file names in the 
\fIarc\fR directories, if any exist, in the mapsets in the user's current 
mapset search path.)

\fBNote:\fR  To use for text attributes, numeric fields with
values >999 may contain no commas or TABs.  Also, the first record
must have all fields filled.
.SH OPTIONS
Program parameters and the flag have the following meanings.
.LP
\fBFlag:\fR
.IP \fB-n\fR 18
Neatline.  Vectors representing a box (neatline) around the input vector
data will be inserted into the output GRASS vector file.
.LP
\fBParameters:\fR
.IP \fBtype\*=\fIname\fR 18
Coverage type.  Either polygon, or line.
.br
Options:  polygon, line
.IP \fBlines_in\*=\fIname\fR 18
ARC/INFO ungenerate lines file; ungenerate format input file containing line or polygon
coordinates.
.IP \fBpoints_in\*=\fIname\fR 18
ARC/INFO ungenerate label-points file; ungenerate format input file containing
label-point coordinates; only applies to 'polygon' type data.
.IP \fBtext_in\*=\fIname\fR 18
ARC/INFO ungenerate label-text file; ungenerate format input file containing
category numbers and (optionally) attribute text.
.IP \fBvector_out\*=\fIname\fR 18
Resultant GRASS vector output file.
.IP \fBidcol\*=\fIvalue\fR 18
ID Number column in label-text file.
Number of label-text column containing line-ID numbers.
.IP \fBcatcol\*=\fIvalue\fR 18
GRASS category column in label-text file.
Number of label-text column containing category values.
.IP \fBattcol\*=\fIvalue\fR 18
GRASS attribute column in label-text file.
Number of label-text column containing attribute text.
.LP
This program can be run either non-interactively or interactively.
The program will run non-interactively
if the user specifies the (optional) flag setting and needed parameter values
on the command line, using the form:
.LP
.RS
\fBv.in.arc \fR[\fB-n\fR] \fBtype\*=\fIname \fBlines_in\*=\fIname \fR[\fBpoints_in\*=\fIname\fR] [\fBtext_in\*=\fIname\fR]
    \fBvector_out\*=\fIname \fR[\fBidcol\*=\fIvalue\fR] [\fBcatcol\*=\fIvalue\fR] [\fBattcol\*=\fIvalue\fR]
.RE
.LP
.LP
Alternately, the user can type:
.LP
.RS
\fBv.in.arc\fR
.RE
.LP
on the command line without program arguments;  in this case,
the program will prompt the user for the flag setting and parameter values
in the manner shown below.

.LP
In ARC/INFO, three files are used to store polygon data:
.br
1) a \fIlines file\fR, which contains coordinates of all the area edge lines;
.br
2) a \fIlabel-point file\fR, which contains coordinates of label-points
(each of which has associated with it a unique label-point ID number).
One label-point is associated with each polygon defined in the \fIlines file\fR;
.br
3) a \fIlabel-text file\fR, which associates each label-point ID number
with a category number and category ("attribute") text.
 
Linear feature data are stored in two files:
.br
1) a \fIlines file\fR, which contains geographic coordinates defining
lines, each with a line-ID number; and
.br
2) a \fIlabel-text file\fR, which associates each line-ID number with
a category number and attribute text.

These data files are described in further detail below, under the DATA FILE FORMATS
section.
.SH INTERACTIVE MODE
The program will prompt the user for the flag setting and parameter values
if the user does not specify these on the command line.
First, the user will be asked to assign a name to the vector file to
store program output:

.nf
      VECTOR (DIGIT) FILENAME
     Enter 'list' for a list of existing binary vector files
     Hit RETURN to cancel request
     >
.fi

Next, the user is asked to specify the COVERAGE (feature) type to be imported
into GRASS.  Valid coverage types are \fBpolygon\fR and \fBline\fR.

.nf
      COVERAGE TYPE
     Enter "polygon" or "line"
     Hit RETURN to cancel request
     >
.fi


IMPORTING A POLYGON COVERAGE

If the user chooses POLYGON coverage, he is asked if he wishes a neatline
placed around his data.  (The existence of neatlines in the output file
can facilitate subsequent patching of data files.)

.nf
      NEATLINE
     Do you want a neatline ?
     Enter "yes" or "no"
     >
.fi

If the user types \fByes\fR, vectors that box the data will be inserted
into the GRASS vector output file (\fIvector_out\fR);
otherwise, no neatline will be inserted into the output file.

Next, the user is prompted for the name of an existing lines-file containing
the geographic coordinates of the arcs forming polygon perimeters.
The lines-file is created with the ARC/INFO \fIUngenerate LINES\fR option,
and is in the same format at the \fIprefix.pol\fR file created by the
\fIv.out.arc\fR program.
The user sees the following prompt:

.nf
      LINES FILENAME
     Enter name of the file created with the LINES
     option of the ARC/INFO Ungenerate command.
     Hit RETURN to cancel request
     >
.fi

The next prompt for coverage type "polygon" asks for the
name of an existing label-points file.
The label-points file is created with the \fIUngenerate POINTS\fR option,
and is in the same format as the \fIprefix.lab\fR file created by the
\fIv.out.arc\fR program.  The user sees the following prompt:

.nf
      LABEL-POINTS FILENAME
     Enter name of file created with the POINTS
     option of the ARC/INFO Ungenerate command.
     Hit RETURN if there is no such file
     >
.fi

Finally, the program asks the user for the name of an existing label-text file.
This file associates each label-point ID number with a text string.
It is in the same format as the \fIprefix.txt\fR file created by the
\fIv.out.arc\fR program.

.nf
      LABEL-TEXT FILENAME
     Enter the name of a file that associates
     label-point ID numbers with text label strings
     Hit RETURN if there is no such file
     >
.fi

.I v.in.arc
then scans the label-text file to find the numbers of lines and columns,
the column headers (if any), and the first three lines of actual data in the file.
It displays this information to standard output
to help the user determine which columns will hold the ID, Category value,
and Attribute text data in the new vector output file.
A sample of the program's output is shown below:

.NF
   The LABEL-TEXT file has been scanned. There are 132
   lines in the file and 8 columns in the file
   
   Column headers of the LABEL-TEXT file:
     rec#\ AREA PERIMETER SOILS#\ SOILS-ID SOIL-CODE DRAIN_CODE TXTUR-CODE
   
   Here are the first three lines :
        1   -2.30228E+07   19399.848     1      0      0      0      0
        2     81079.875    1678.826     2      1     15      3      3
        3    955952.500   10229.637     3      2     19      8      8
.FI

The column of category values must contain only integer values.
The attribute text column can contain a floating point number, an integer,
or a word (text string).
.br

Finally, the user is prompted to enter line ID, category value,
and attribute text column numbers.

.nf
	
	Enter the number of the column that should be used
	for line IDs (probably the column with -ID) :
.fi

.nf
     Enter the number of the column that is to be used
     for GRASS category values:
.fi


.nf
     Enter the number of the column that should be used
     for GRASS attribute text:
.fi

Once these column numbers have been entered,
.I v.in.arc
will begin converting the ARC/INFO "Generate" format files into GRASS vector file format.


IMPORTING A LINE COVERAGE

The user will also be prompted for input when importing ARC/INFO
files containing linear features ("line coverage") data.
Like polygon data, linear features are constructed by the series of
arcs (aka, vectors) defining their perimeters.
If the user selects LINE coverage, the prompts seen by the user will be different
in two respects from those for POLYGON coverage.  First, the user will not be asked
whether or not a neatline is desired;  and second, no label-points file name
will be requested.  In other respects, the treatment of LINE coverage is
identical to that for POLYGON coverage.
.LP
The user is prompted for the name of the lines-file containing
the geographic coordinates of these arcs.  The lines-file must
first have been created with the ARC/INFO \fIUngenerate LINES\fR
option, and is in the same format as the \fIprefix.lin\fR file created
by the GRASS \fIv.out.arc\fR program.

.SH DATA FILE FORMATS
Following are examples of the data files discussed above.

LINES FILE, also known as \fIprefix.lin\fR or \fIprefix.pol\fR file.
.br
This type of file can be created in ARC/INFO by using the \fIlines\fR
subcommand of the \fIUngenerate\fR command.   Each line (aka, arc)
is defined by a line-ID number, followed by a list of at least two
easting and northing coordinate pairs, followed by a line with the
word "END".  The file is terminated with the word "END".

The line-ID number is important only for line coverage data.  For a line 
coverage, the line-ID number is the number that associates each line with
its attribute data.

.nf
.TS
c c
l l.
3
711916.000000	4651803.000000
711351.875000	4651786.000000
.T&
l c
c c
l l.
END
3
709562.500000	4651731.000000
709617.250000	4651624.000000
709617.250000	4651567.000000
709585.000000	4651503.000000
709601.125000	4651470.000000
709696.875000	4651503.000000
709720.500000	4651574.000000
709823.750000	4651575.000000
709893.125000	4651741.000000
.T&
l c
c c
l l.
END
3
710296.875000	4651491.000000
710295.125000	4651470.000000
710223.000000	4651454.000000
710154.500000	4651463.000000
.T&
l c.
END
END
.TE

LABEL-POINTS FILE, also known as \fIprefix.lab\fR file.
.br
This type of file can be created in ARC/INFO using the \fIPoints\fR
option of the \fIUngenerate\fR command.
The first number on each line is a label-point ID number, and the 
following two numbers are (respectively) the easting and northing
coordinate pair representing the geographic location of the label-point.

.nf
.TS
l r r.
1	711539.875000	4651743.000000
2	711429.000000	4650632.000000
3	711027.625000	4651736.000000
4	711022.625000	4651519.000000
5	710482.750000	4651494.000000
6	710474.500000	4651667.000000
7	709269.750000	4651018.000000
8	709726.500000	4651604.000000
9	708926.375000	4651195.000000
10	708567.500000	4651644.000000
11	708272.750000	4651407.000000
END
.TE
.fi

LABEL-TEXT FILE, also known as \fIprefix.txt\fR file.
.br
The ARC/INFO \fIDisplay\fR command can be used to create this type of file.

.nf
.TS
l r r r r r r.
1	-2.30228E+07	19399.848	1	0	0	0	
2	81079.875	1678.826	2	1	15	3
3	955952.500	10229.637	3	2	19	8
4	41530.875	926.887	4	3	17	3
5	87900.188	1900.909	5	4	13	3
6	166125.125	3512.950	6	5	15	3
7	29460.563	824.968	7	6	17	3
8	1022769.875	9105.707	8	7	20	9
9	51385.500	1075.638	9	8	17	3
10	376834.875	4470.027	10	9	9	2
11	65802.688	1575.088	11	10	16	3
.TE
.fi
.SH	NOTES

ARC/INFO data can be imported even if a label-points and/or a label-text file
are missing;  however, the lines and/or areas imported will not be labeled.

\fIv.in.arc \fR
can handle label-text files both with and without header lines.
.SH SEE ALSO
.I v.out.arc,
.I v.support
.SH AUTHOR
.nf
Dave Johnson
DBA Systems, Inc.
10560 Arrowhead Drive
Fairfax, Virginia 22030
.fi
