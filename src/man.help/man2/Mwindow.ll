./" %W% %G%
.TH Mwindow.ll 2M GRASSmapdev
.nh
.SH NAME
Mwindow.ll \- Converts current 
.CT window "geographic window"
information 
from UTM to geographic coordinates
.br
.I (Map Development Tool)
.SH SYNOPSIS
.B Mwindow.ll 
.I spheroid
.SH DESCRIPTION
.I Mwindow.ll
takes the current 
.CT window "geographic window"
.XT location 
information, 
which is in UTMs, and converts it to geographic coordinates
stated in latitudes and longitudes.
It also prints the length (in meters) of one arc second at
each of the four edges of the 
.CT "window." "geographic window."
 The user must enter
the spheroid upon which to base the geographic coordinates.

The list of spheroids available is somewhat dynamic. It may not
contain exactly the ones listed below. To determine the current
list of possible spheroids, simply type in:

.ti +.5i
Mwindow.ll help

A list of available spheroids will be
printed on the screen. If the spheroid desired is not on the 
list, the values for the semi-major axis and the eccentricity
squared for the spheroid may be entered in place of a spheroid name in the
following format:
.sp .25
.ti +.5i
s\*=a\*=\fIsemi-major_axis\fR,e\*=\fIeccentricity_squared\fR

SOME POSSIBLE SPHEROIDS
.br
(\fIThe on-line listing includes only the spheroid names\fR)
.sp .25
.TS
c|c|l|l
l|l|l|l.
Spheroid	Commonly used for:	Semi-major axis	Eccentricity sqrd
_
australian	Australia	6378160.0	0.0066945419
bessel	Japan	6377739.155	0.0066743722
clark66	N. America	6378206.4	0.006768658
clark80	France, Africa	6378249.145	0.0068035113
everest	India, Burma	6377276.345	0.0066378466
international	Europe	6378388.0	0.00672267
wgs72	worldwide coverage	6378135.0	0.006694317778
.TE

.SH EXAMPLE
.B "Mwindow.ll clark66"

Results:
.in +.5i
.TS
tab(#);
l r r l.
WINDOW#4928000.00N#609000.00E#ZONE 13
#4914000.00S#590000.00W
.TE
 
.TS
tab(#);
r r.
44.30.06N#44.29.57N
103.52.04W#103.37.44W

44.22.32N#44.22.23N
103.52.13W#103.37.55W
.TE

at northern edge 1 arc second longitude\*=22.088500m
.br
at southern edge 1 arc second longitude\*=22.135998m
.br
at western edge 1 arc second latitude\*=30.860285m
.br
at eastern edge 1 arc second latitude\*=30.863082m

.in -.5i
The values for the geographic coordinates are rounded to the
nearest second in this example. They would be more precise
in the actual output that is printed on the screen.
.SH "SEE ALSO"
\fIMimport.ll[2M], Mll2u[2M], Mu2ll[2M]\fR
.SH AUTHOR
Michael Shapiro, U.S. Army Construction Engineering Research Laboratoy
