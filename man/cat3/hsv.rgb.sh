


hsv.rgb.sh <scripts> GRASS Reference Manual  <scripts> hsv.rgb.sh



NAME
     hsv.rgb.sh	- Converts HSV (hue, saturation, and value) cell
     values to RGB (red, green,	and blue) values.
     (GRASS Shell Script)

SYNOPSIS
     hsv.rgb.sh	file1 file2 file3

DESCRIPTION
     hsv.rgb.sh	is a Bourne shell (sh(1)) program that converts
     HSV to RGB	using r.mapcalc.  The Foley and	Van Dam	algorithm
     is	the basis for this program.  Input must	be three raster
     files - each file supplying the HSV values.  Three	new
     raster files are created representing RGB.

NOTES
     Do	not use	the same names for input and output.

FILES
     This program is simply a shell script stored in the file
     hsv.rgb.sh	under the $GISBASE/scripts directory.  Users are
     encouraged	to make	their own shell	script programs	using
     similar techniques.

SEE ALSO
     rgb.hsv.sh

AUTHOR
     James Westervelt, U.S. Army Coonstruction Engineering
     Research Laboratory

























GRASS 4.2		Baylor University			1



