.TH shade.rel.sh
.SH NAME
\fIshade.rel.sh\fR\ \- Creates a shaded relief map based on current
resolution settings and sun altitude and azimuth values entered
by the user.
.br
.I "(GRASS Shell Script)"
.SH SYNOPSIS
.B shade.rel.sh
.SH DESCRIPTION
.I shade.rel.sh
is a Bourne shell (sh(1)) script that creates a raster shaded relief map
based on current resolution settings and on sun altitude and azimuth
values entered by the user.  The new shaded relief map map is named
\fIshade\fR and stored in the user's current mapset.  The map is assigned
a grey-scale color table.
.LP
This program is interactive;  the user enters the command:
.LP
.RS
\fBshade.rel.sh\fR
.RE

The program then prompts the user to enter values for:
.RS
(1)  the altitude of the sun in degrees above the horizon
(a value between 0 and 90 degrees), and
.br
(2)  the azimuth of the sun in degrees to the east of north
(a value between -1 and 360 degrees).
.br
(3)  the name of a raster map layer whose cell category values are to
provide elevation values for the shaded relief map.  Typically, this
would be a map layer of elevation;  however, any raster map layer
can be named.
.RE

Specifically, \fIshade.rel.sh\fR executes the following \fIr.mapcalc\fR
statement:
.LP
.nf
.B
.RS
r.mapcalc << EOF
shade = eval( \\
 x=($elev[-1,-1] + 2*$elev[0,-1] + $elev[1,-1] \\
   -$elev[-1,1] - 2*$elev[0,1] - $elev[1,1])/(8.*$ewres) , \\
 y=($elev[-1,-1] + 2*$elev[-1,0] + $elev[-1,1] \\
   -$elev[1,-1] - 2*$elev[1,0] - $elev[1,1])/(8.*$nsres) , \\
 slope=90.-atan(sqrt(x*x + y*y)), \\
 a=round(atan(x,y)), \\
 aspect=if(x||y,if(a,a,360)), \\
 cang = sin($alt)*sin(slope) + cos($alt)*cos(slope) * cos($az-aspect), \\
 if(cang < 0,0,100*cang))
EOF
.RE
.R
.fi
.LP
Refer to the manual entry for \fIr.mapcalc\fR for an explanation
of the filtering syntax shown in the above expression.
See, for example, the section on "The Neighborhood Modifier".
.LP
\fIshade.rel.sh\fR then runs \fIr.colors\fR to assign a grey-scale color
table to the new shaded relief map \fIshade\fR, by executing the command:
.LP
.RS
\fBr.colors shade color=grey \fR
.RE
.SH FILES
This program is simply a shell script.  Users are encouraged to make their own
shell scripts using similar techniques.  See $GISBASE/scripts/shade.rel.sh.
.SH SEE ALSO
"\fIr.mapcalc\fR:  An Algebra for GIS and Image Processing,"
by Michael Shapiro and Jim Westervelt,
U.S. Army Construction Engineering Research Laboratory (March/1991).
.LP
"GRASS Tutorial:  \fIr.mapcalc\fR,"
by Marji Larson, U.S. Army Construction Engineering Research Laboratory.
.LP
.I blend.sh,
.I g.ask,
.I g.region,
.I r.colors,
and
.I r.mapcalc
.SH AUTHOR
Jim Westervelt, U.S. Army Construction Engineering Research Laboratory
