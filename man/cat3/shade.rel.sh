


shade.rel.sh <scripts>GRASS Reference Manua<scripts> shade.rel.sh



NAME
     shade.rel.sh - Creates a shaded relief map	based on current
     resolution	settings and sun altitude and azimuth values
     entered by	the user.
     (GRASS Shell Script)

SYNOPSIS
     shade.rel.sh

DESCRIPTION
     shade.rel.sh is a Bourne shell (sh(1)) script that	creates	a
     raster shaded relief map based on current resolution
     settings and on sun altitude and azimuth values entered by
     the user.	The new	shaded relief map map is named shade and
     stored in the user's current mapset.  The map is assigned a
     grey-scale	color table.

     This program is interactive;  the user enters the command:

	  shade.rel.sh

     The program then prompts the user to enter	values for:
	  (1)  the altitude of the sun in degrees above	the
	  horizon (a value between 0 and 90 degrees), and
	  (2)  the azimuth of the sun in degrees to the	east of
	  north	(a value between -1 and	360 degrees).
	  (3)  the name	of a raster map	layer whose cell category
	  values are to	provide	elevation values for the shaded
	  relief map.  Typically, this would be	a map layer of
	  elevation;  however, any raster map layer can	be named.

     Specifically, shade.rel.sh	executes the following r.mapcalc
     statement:

	  r.mapcalc << EOF
	  shade	= eval(	\
	   x=($elev[-1,-1] + 2*$elev[0,-1] + $elev[1,-1] \
	     -$elev[-1,1] - 2*$elev[0,1] - $elev[1,1])/(8.*$ewres) , \
	   y=($elev[-1,-1] + 2*$elev[-1,0] + $elev[-1,1] \
	     -$elev[1,-1] - 2*$elev[1,0] - $elev[1,1])/(8.*$nsres) , \
	   slope=90.-atan(sqrt(x*x + y*y)), \
	   a=round(atan(x,y)), \
	   aspect=if(x||y,if(a,a,360)),	\
	   cang	= sin($alt)*sin(slope) + cos($alt)*cos(slope) *	cos($az-aspect), \
	   if(cang < 0,0,100*cang))
	  EOF

     Refer to the manual entry for r.mapcalc for an explanation
     of	the filtering syntax shown in the above	expression.  See,
     for example, the section on "The Neighborhood Modifier".





GRASS 4.2		Baylor University			1






shade.rel.sh <scripts>GRASS Reference Manua<scripts> shade.rel.sh



     shade.rel.sh then runs r.colors to	assign a grey-scale color
     table to the new shaded relief map	shade, by executing the
     command:

	  r.colors shade color=grey

FILES
     This program is simply a shell script.  Users are encouraged
     to	make their own shell scripts using similar techniques.
     See $GISBASE/scripts/shade.rel.sh.

SEE ALSO
     "r.mapcalc:  An Algebra for GIS and Image Processing," by
     Michael Shapiro and Jim Westervelt, U.S. Army Construction
     Engineering Research Laboratory (March/1991).

     "GRASS Tutorial:  r.mapcalc," by Marji Larson, U.S. Army
     Construction Engineering Research Laboratory.

     blend.sh, g.ask, g.region,	r.colors, and r.mapcalc

AUTHOR
     Jim Westervelt, U.S. Army Construction Engineering	Research
     Laboratory































GRASS 4.2		Baylor University			2



