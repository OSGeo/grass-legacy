


m.region.ll <main>   GRASS Reference Manual    <main> m.region.ll



NAME
     m.region.ll - Converts Universal Transverse Mercator (UTM)
     coordinates falling within	the current geographic region
     from UTM coordinates to geographic	(latitude/longitude)
     coordinates.
     (GRASS Data Import/Processing)

SYNOPSIS
     m.region.ll
     m.region.ll help
     m.region.ll spheroid=name

DESCRIPTION
     m.region.ll takes current geographic region settings in UTM
     coordinates, and converts them to geographic coordinates
     (i.e., latitudes and longitudes).	It also	prints the length
     (in meters) of one	arc-second at each of the four edges of
     the geographic region.  The user must enter the spheroid
     upon which	to base	the geographic coordinates.

     The list of spheroids available is	somewhat dynamic.  It may
     not contain exactly the ones listed below.	 To determine the
     current list of possible spheroids, simply	type in:

	  m.region.ll help

     A list of available spheroids will	be printed on the screen.

     AVAILABLE SPHEROIDS
     (The on-line listing includes only	the spheroid names.)
     Spheroid:					  |    Semi-major axis	|  Eccentricity	sqrd (e),|  Commonly used for:
						  |  (Equatorial Radius)|  Flattening (f),	 |
						  |  (a):		|  or Polar Radius (b):	 |
     _____________________________________________|_____________________|________________________|____________________
     airy	   a=6377563.396 e=.006670540	  |			|			 |
     australian	    a=6378160	  f=1/298.25	  |  Australia		|			 |
     bessel	   a=6377397.155 e=.006674372	  |  Japan		|			 |
     clark66	   a=6378206.4	 b=6356583.8	  |  N.	America		|			 |
     everest	   a=6377276.345 e=.0066378466	  |  India, Burma	|			 |
     grs80	   a=6378137	 f=1/298.257	  |			|			 |
     hayford	   a=6378388	 f=1/297	  |			|			 |
     international a=6378388	 f=1/297	  |  Europe		|			 |
     krasovsky	   a=6378245	 f=1/298.3	  |			|			 |
     wgs66	   a=6378145	 f=1/298.25	  |  worldwide coverage	|			 |
     wgs72	   a=6378135	 f=1/298.26	  |  worldwide coverage	|			 |
     wgs84	   a=6378137	 f=1/298.257223563|  worldwide coverage	|			 |

EXAMPLE
	  m.region.ll spheroid=clark66

     Results:
	  WINDOW   4928000.00N	 609000.00E   ZONE 13



GRASS 4.2		Baylor University			1






m.region.ll <main>   GRASS Reference Manual    <main> m.region.ll



		   4914000.00S	 590000.00W

      44.30.06N	   44.29.57N
     103.52.04W	  103.37.44W

      44.22.32N	   44.22.23N
     103.52.13W	  103.37.55W

     At	northern edge 1	arc-second longitude=22.088500m
     At	southern edge 1	arc-second longitude=22.135998m
     At	western	edge 1 arc-second latitude=30.860285m
     At	eastern	edge 1 arc-second latitude=30.863082m

The values for the geographic coordinates are rounded to the
nearest	second in this example.	  They would be	more precise in
the actual output that is printed on the screen.

SEE ALSO
     m.datum.shift, m.ll2u, m.u2ll, r.in.ll

AUTHOR
     Michael Shapiro, U.S. Army	Construction Engineering Research
     Laboratory
































GRASS 4.2		Baylor University			2



