.TH grass.logo.sh
.SH NAME
\fIgrass.logo.sh\fR \- Displays a GRASS/Army Corps of Engineers
logo in the active display frame on the graphics monitor.
.br
.I "(GRASS Shell Script)"
.SH SYNOPSIS
.B grass.logo.sh
.SH DESCRIPTION
.I grass.logo.sh
is primarily a demonstration of the GRASS
.I d.graph
program in a UNIX Bourne shell macro
that generates the U.S. Army Corps of Engineers logo.
Users are encouraged to generate their own unique logos by writing similar
macro shell scripts.  Examine the contents of the input file
called \fIgrass.logo.sh\fR located in the GRASS shell script command directory
(\fI$GISBASE/scripts\fR) to see how
the GRASS logo was generated using \fId.graph\fR graphics commands.
The coordinates for this logo were taken from a drawing done on graph paper.
.LP
To view the graphics described by this file use \fId.graph \fRor
\fId.mapgraph\fR, making sure that a copy of this file is either in your
current directory or is given by its full path name. 
.SH "NOTES"
\fIgrass.logo.sh\fR, like \fId.rast\fR, will overwrite (not overlay)
whatever display appears in the active graphics frame.
.LP
This program requires no command line arguments.
.SH "SEE ALSO"
.I d.font,
.I d.graph,
.I d.mapgraph,
.I d.rast
.SH "AUTHOR"
James Westervelt, U.S. Army Construction Engineering Research Laboratory
