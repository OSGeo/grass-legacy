.TH slide.show.sh
.SH NAME
\fIslide.show.sh \fR\- Displays a series of raster map layers
existing in the user's current mapset search path on the graphics monitor.
.br
.I "(GRASS Shell Script)"
.SH SYNOPSIS
\fBslide.show.sh \fR[\fBacross\*=\fIvalue\fR] [\fBdown\*=\fIvalue\fR] [\fBmapsets\*=\fIlist\fR]
.SH DESCRIPTION
.I slide.show.sh 
is a UNIX Bourne shell macro which clears the entire screen,
creates a series of display frames on the graphics monitor,
and displays in slideshow format each of the raster map layers listed
in the user-specified \fImapsets\fR.
This is a shell script example which makes extensive use
of GRASS and UNIX commands.  Users are encouraged to examine this macro
and develop similar on-line demos using their own data files.

.LP
\fBParameters:\fR
.IP "\fBacross\*=\fIvalue\fR" 20
The number of display frames across the graphics monitor screen
to be used for map display.
.br
Default:  4
.IP "\fBdown\*=\fIvalue\fR" 20
The number of display frames down the graphics monitor screen
to be used for map display.
.br
Default:  3
.IP "\fBmapsets\*=\fIlist\fR" 20
The names of the mapsets under the user's current location
whose raster map layers are to be displayed, separated by commas.
.br
Default:  All mapsets listed in the user's current mapset search path.
.SH FILES
See the file \fIslide.show.sh\fR under $GISBASE/scripts.
.SH "SEE ALSO"
.I d.display,
.I d.erase,
.I d.text,
.I g.mapsets,
.I 3d.view.sh,
.I demo.sh,
.I grass.logo.sh,
.I show.fonts.sh
.SH "AUTHOR"
James Westervelt, U.S. Army Construction Engineering Research Laboratory
