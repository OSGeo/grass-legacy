


rgb.hsv.sh <scripts> GRASS Reference Manual  <scripts> rgb.hsv.sh



NAME
     rgb.hsv.sh	- Converts RGB (red, green, and	blue) cell values
     to	RGB (red, green, and blue) values.
     (GRASS Shell Script)

SYNOPSIS
     rgb.hsv.sh	file1 file2 file3

DESCRIPTION
     rgb.hsv.sh	is a Bourne shell (sh(1)) program which	converts
     RGB values	to HSV using r.mapcalc.	 The Foley and Van Dam
     algorithm is the basis for	the program.  Input must be three
     raster files - each file supplying	the RGB	values.	 Three
     new raster	files are created representing HSV.

NOTES
     Do	not use	the same names for input and output.

FILES
     This program is simply a shell script stored under	the
     $GISBASE/scripts directory.  The user is encouraged to
     examine the shell script programs stored here and to produce
     other such	programs.

SEE ALSO
     hsv.rgb.sh

AUTHOR
     James Westervelt, U.S. Army Construction Engineering
     Research Laboratory

























GRASS 4.2		Baylor University			1



