


m.geo <contrib>	      GRASS Reference Manual	  <contrib> m.geo



NAME
     m.geo - Calculates conversion coordinates for geographic
     positions.
     (SCS GRASS Map Development Program)

SYNOPSIS
     m.geo
     m.geo help

DESCRIPTION
     This program allows a user to interactively: convert
     projection coordinates Northings and Eastings to Latitude
     and Longitude values,  or

     allows a user to interactively: convert Latitude and
     Longitude values to projection coordinate Northings and
     Eastings.

     It allows a user to do all of the above: reading from a
     file, writing to the screen,  or reading from the keyboard,
     writing to a file, or reading from a file, writing to a
     file,

     Note:  The program does not transform GRASS files, it is
     designed to determine coordinate values on an individual
     position.

     Several map projections are currently supported:
     stp - State Plane, for all CONUS, Alaska, Hawaii, Puerto
     Rico, Guam, Virgin Islands, American Samoa, Northern Mariana
     Islands, Palau, and U.S. Minor Outlying Islands
     F
     I
     F
     I
     F
     F

     F
     I
     F
     I

     I
     I
     I
     I

     F

     I	       I
     I	       I



6				 m				 1






>				 l				 o



     I	       I
     F
     I
     I

     F

     I
     F
     I

     F
     I

     F
     I
     I
     I
     I

     I
     I
     I
     I
     I
     I

     F
     I

     I
     I
     F
     I

     I
     F
     I
     I
     I

     I

     F
     I

     I
     I

     I
     I
     I



2				 m				 6






>				 l				 o



     I

     I
     I

     I
     F
     F
     F

     F
     I
     F

     Each of the above projections (with the exception of State
     Plane) can be computed with the following spheroids:
     3
     )
     6
     0
     4
     )
     1
     6
     .
     0
     h
     )
     2
     0
     y
     t
     8
     7
     a
     k
     6
     2
     m


INPUT FILE FORMAT
     When reading from a file of LATITUDE/LONGITUDE data the file
     will contain three (3) columns of information:
     the first column - latitude   - in degrees minutes seconds,
     the second column - longitude - in degrees minutes seconds,
     the third column - zone	   - zero(0) if not required.

     For example:
	       +40 36 31.4563	-87 2 7.8193   16
	       40n 36 31.4563	87w 2 7.8193   16




GRASS 5.0beta6	      GRASS Development Team			3






m.geo <contrib>	      GRASS Reference Manual	  <contrib> m.geo



     When reading from a file of PROJECTION COORDINATES data the
     file will contain three (3) columns of information:
     the first column - easting - ground coordinates
     the second column - northing - ground coordinates
     the third column - zone	   - zero(0) if not required.

     For example:
	       500000.00    4496918.64	 16   <- utm
	       -424489.11   1908736.13	 0    <- lambert

     Note: NO column headings are required, just the numbers.

SEE ALSO
     Mapgen, proj

AUTHOR
     R.L. Glenn, USDA, SCS, NHQ-CGIS


NOTICE
     This program is part of the contrib section of the GRASS
     distribution.  As such, it is externally contributed code
     that has not been examined or tested by the Office of GRASS
     Integration.































4		      GRASS Development Team	   GRASS 5.0beta6



