.\" %W% %G%
.TH i.tape.tm 1 GRASSimagery
.nh
.SH NAME
.IP i.tape.tm\ \- 13
An imagery function that extracts Thematic Mapper imagery from half inch tape
.br
.I (GRASS Imagery Tool)
.SH SYNOPSIS
.B i.tape.tm
.SH DESCRIPTION
.I i.tape.tm
is a program that extracts Thematic Mapper (TM) imagery from tape.

This program must be run in a LOCATION with a x,y coordinate
system (i.e., a coordinate system with projection 0).
For further information regarding 
this LOCATION refer to the manual entry \fIimagery[5]\fR.

The first prompt in \fIi.tape.tm\fR asks the user for the 
tape device name.  This is sometimes  /dev/rmt0 (for a density of 1600),
but this varies with each machine.

The next prompt is:

.if t \fB
.nf
Please mount and load tape, then hit RETURN -->
.fi
\fR
.SH IMAGE IDENTIFICATION MENU
The first menu in the program asks the user for information about the data.

.if t \fB
.TS
center, tab(|);
c s
l l.
please enter the following information

Tape Identification:|_

Image Description:|_

Title for the Extracted Cell Files:|_

.T&
c s.
AFTER COMPLETING ALL ANSWERS, HIT <ESC> TO CONTINUE
(OR <Ctrl-C> TO CANCEL)
.TE
\fR

This program automatically enters the scene ID number into
the field for Tape Identification.
The mission, path, row, quadrant, date, and whether the image
is corrected is automatically entered into the field for Image 
Description.

The second menu is:

.ne 12
.ce 2
.if t \fB
THEMATIC MAPPER EXTRACT
please select the desired tape window to extract
.nf
.ce 2

first row: _______(1-2984)
last row: _______(1-2984)
.fi
.nf
.ce 2

first col: _______(1-4220)
last col: _______(1-4220)

.fi

.ce 2
AFTER COMPLETING ALL ANSWERS, HIT <ESC> TO CONTINUE
(OR <Ctrl-C> TO CANCEL)\fR

The numbers in parentheses are the total number of rows and columns
on the tape including zeros (filler).  This information and additional
information can also be obtained by running the program \fIexamine.tape\fR.
\fIexamine.tape\fR will read any tape and provide the user with
the number of files on a tape, the number of records on a tape, and
the record lengths.
Any subset of the image on the tape may be extracted.
For a discussion of row and column extraction see the subheading
titled ROW AND COLUMN EXTRACTION below.

The next menu is:

.ne 12
.ce 1
.if t \fB
please make an x by the bands you want extracted
.nf
.ce 7

_____ 1
_____ 2
_____ 3
_____ 4
_____ 5
_____ 6
_____ 7

.fi
.ce 2
AFTER COMPLETING ALL ANSWERS, HIT <ESC> TO CONTINUE
(OR <Ctrl-C> TO CANCEL)\fR

TM imagery has 7 bands, but the user may want to extract 
only a subset of these bands.  See the subheading in this entry
titled ROW AND COLUMN EXTRACTION.

The user then is asked to enter the prefix/group for the band cell 
.CT files maps
to be created.  This name will precede each band 
.CT file  "cell map"
extracted into GRASS.  For example, if 3 bands are extracted 
the following 3 band 
.CT files "cell maps"
will result:

.ce 3
.I prefixname.1
.br
.I prefixname.2
.br
.I prefixname.3

The specified \fIprefixname\fR
will also automatically become the name for the imagery group 
file being created.  Each image or quad (i.e., each run of \fIi.tape.tm\fR)
should be given a unique prefix/group name.

.TE
The extraction process will begin by first skipping the number
of specified files, advancing to the first band requested, and then 
reading the tape.
After extracting the requested rows and columns for each band, the
program creates support files for the 
.CT "band." "band cell map."
The percent completion of the extraction is displayed
on the screen.  Because TM imagery is divided into 4 quads and is  
stored in multiple tape sets, the program is designed to read
one quad at a time.  The number of tapes required to store one
quad depends on the number of bytes per inch in which the data is stored.
If more than one tape is required to store one quad, the program will pause
and inform the user to mount the next tape.

The extracted band 
.CT files "cell maps"
will be listed as cell 
.CT files maps
available in the 
current MAPSET and may be displayed using the GRASS commands
\fIdisplay\fR, \fIDcell\fR or \fIi.points\fR.
.SH NOTES
After extracting an image from tape the 
.CT "" geographic
window in the x,y coordinate LOCATION will be set based upon the 
extracted rows and columns from the tape. The relationship between
the image rows and columns and the 
.CT "" geographic
window 
.CT "" map
coordinates is discussed in 
the manual entry \fIimagery[5]\fR.

.SH ROW AND COLUMN EXTRACTION
The display options in 
.CT "GRASS-GRID" "GRASS"
allow the user to locate rows and
columns on the digital image.  If enough disk space is available, one
band of an entire image or, one band of a portion of an image known to 
contain the area of interest, can be extracted and displayed.  The 
.I measurements 
option in \fIdisplay\fR, or \fIDwhere\fR (following use of \fIDcell\fR)
will echo x and y coordinates to the screen.  (These coordinates will
display negative numbers in the north-south direction, but ignoring the
negative sign will yield the row number.)  See manual entry \fIimagery[3]\fR
for further explanation.

Each quad of a TM image contains filler on both the 
left and right sides of the quad.  The user may want to identify the row and
column numbers in order to exclude the filler. This filler will otherwise be 
extracted with the image and take up unnecessary disk space.

If a photograph of the digital image is available, the rows and columns
to be extracted can be determined from it by associating inches with the 
total number of known rows and columns in the scene.  For example, if 
the total length of the photograph is 12 inches, the total number of rows
on the tape is 2000, and the northwest corner of the area of interest 
begins 2 inches from the top of the photo, then:

.ce 2
12" / 2000 rows = 2" / x rows
x = 333.333

The northwest corner of the area of interest starts at row 333.  The
starting row, ending row, starting column, and ending column can be 
calculated in this manner.

.SH SEE ALSO
.nf
\fIGRASS Tutorial: Image Processing
\fIdisplay[1]
\fIDcell[2D], Dwhere[2D]
\fIexamine.tape[1], i.group[1], i.points[1]
\fIimagery[3]\fR
.fi
.SH AUTHOR
Michael Shapiro, U.S. Army Construction Engineering Research Laboratory
