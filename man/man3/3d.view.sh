.TH 3d.view.sh
.SH NAME
\fI3d.view.sh\fR \- Displays several 3-dimensional views of a landscape
on the user's graphics monitor.
.br
\fI(GRASS Shell Script)\fR
.SH SYNOPSIS
\fB3d.view.sh\fR
.br
\fB3d.view.sh help\fR
.br
\fB3d.view.sh
\fBfile\*=\fImapname  \fBef\*=\fImapname  \fBvh\*=\fIviewing_height 
\fBsv\*=\fIsink_value  \fBexag\*=\fIexag  \fBlf\*=\fIline_frequency  \fBback\*=\fIbackground_color\fR
.SH DESCRIPTION
.I 3d.view.sh
is a Bourne shell (sh(1)) script that displays several 3-dimensional
views of a landscape on the user's graphics monitor.
It erases the graphics monitor and then prepares it for the
display of nine equally-sized frames.  The user-specified raster
map layer (given by file\*=\fIname\fR) is displayed using
.I d.rast
in the middle frame.  The remaining frames are then used to display 3-d
perspective views.  The top middle panel is a view from the north, the top
right from the north-east, the right from the east, and so on.  Each is drawn
with a call to the
.I d.3d
program.
The viewing angles are calculated automatically.  If options are not stated
on the command line, default values will be used.  These values are listed
under Parameters, below.
.LP
\fBParameters:\fR
.IP \fBfile\*=\fImapname\fR 18
Name of raster map layer to be displayed.
.br
Default:  \fIelevation\fR
.IP \fBef\*=\fImapname\fR 18
Name of raster map layer whose category values will supply the elevation
values used to generate 3-d perspective views.
.br
Default:  \fIelevation\fR
.IP \fBvh\*=\fIviewing_height\fR 18
Height (in meters) of the location from which scenes will be viewed.
.br
Default:  30000
.IP \fBsv\*=\fIsink_value\fR 18
Sink factor value, causing the image to be displayed lower, or higher,
on the graphics screen.
.br
Default:  0
.IP \fBexag\*=\fIvertical_exaggeration\fR 18
Vertical exaggeration factor of the values in the elevation file.
.br
Default:  3
.IP \fBlf\*=\fIline_frequency\fR 18
Contour intervals at which vector grid lines will be drawn, in meters.
.br
Default:  20
.br
.IP \fBback\*=\fIbackground_color\fR 18
Color of the background of the display frames.
.br
Options:  red, orange, yellow, green, blue, indigo, violet,
magenta, brown, gray, white, and black
.br
Default:  black
.SH "NOTES"
In the \fIspearfish\fR sample data base, the user must specify a viewing
height when running \fI3d.view.sh\fR.  Note also that the raster elevation
map layers in the PERMANENT mapset under spearfish are named
\fIelevation.dem\fR and \fIelevation.dma\fR.
.LP
This program will not prompt the user for inputs;  if the user types
\fB3d.view.sh\fR without program arguments on the command line,
default values will be used.
.SH FILES
This program is simply a shell script.  Users are encouraged to make their own
shell scripts using similar techniques.  See $GISBASE/scripts/3d.view.sh.
.SH "SEE ALSO"
.I d.3d,
.I d.rast
.SH "AUTHOR"
James Westervelt, U.S. Army Construction Engineering Research Laboratory
