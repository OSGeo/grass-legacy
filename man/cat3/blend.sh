


blend.sh <scripts>   GRASS Reference Manual    <scripts> blend.sh



NAME
     blend.sh -	Combines the red, green, and blue color
     components	of two raster map layers.
     (GRASS Shell Script)

SYNOPSIS
     blend.sh file1 file2 perc outbase

DESCRIPTION
     blend.sh is a Bourne shell	(sh(1))	script that extracts the
     red (R), green (G), and blue (B) color components from each
     of	two raster map layers, and creates three new raster map
     layers whose category values respectively represent the
     combined red, combined blue, and combined green color values
     from the two input	layers.	 Category values in each of the
     output map	layers will fall within	the range of 0 - 255.

     The R,G,B values from the two input map layers (file1 and
     file2) are	not simply added together, but are instead
     combined by a user-named percentage (perc)	of the R,G,B
     values in file1.  Specifically, blend.sh executes three
     r.mapcalc statements that:	(1) convert the	R,G,B values in
     file1 and file2 to	the range 0 - 255; (2) multiply	the R, G,
     and B values in file1 by a	user-named percentage (perc); (3)
     multiply the R, G,	and B values in	file2 by (100 -	perc);
     (4) create	three new raster map layers, whose category
     values represent the summed R, summed G, or summed	B values
     resulting from (2)	and (3).  Resulting R, G, and B	values
     will respectively be stored in three new raster map layers
     named outbase.r, outbase.g	and outbase.b.

     This program runs non-interactively;  the user must state
     all parameter values on the command line.


     Parameters:

     file1	       Name of a first raster map layer, whose R,
		       G, and B	color components will be combined
		       with those of the second	raster map layer
		       (file2) named.  The percent value (perc)
		       given will apply	to file1.

     file2	       Name of a second	raster map layer, whose
		       color components	will be	combined with
		       those of	file1.	The percent value (perc)
		       given will apply	to the R,G,B values in
		       file1.  The R, G, and B values in file2
		       will be multiplied by (100 - perc) %.

     perc	       Percentage or amount of the color
		       contribution in terms of	color intensity.



GRASS 4.2		Baylor University			1






blend.sh <scripts>   GRASS Reference Manual    <scripts> blend.sh



		       This value is multiplied	by the R,G,B
		       values in file1.

     outbase	       The root	name assigned to each of the
		       three output files created.  A suffix is
		       added to	each file name,	indicating which
		       hold the	red, green, and	blue color
		       values.

     blend.sh executes three r.mapcalc statements:

	  r.mapcalc "outbase.r = r#file1 * .perc + (1.0	- .perc) * r#file2"
	  r.mapcalc "outbase.g = g#file1 * .perc + (1.0	- .perc) * g#file2"
	  r.mapcalc "outbase.b = b#file1 * .perc + (1.0	- .perc) * b#file2"

     It	uses the # operator to separately extract the red, green,
     and blue components in the	named raster map layers,
     essentially allowing color	separates to be	made.

EXAMPLE
     Typing the	following at the command line:

	  blend.sh aspect elevation 40 elev.asp

     will create three new raster map layers named elev.asp.r,
     elev.asp.g, and elev.asp.b, that, respectively, contain 40
     of	the red, green,	and blue components of the elevation map
     layer and contain 60 of the red, green, and blue components
     of	the aspect map layer.

FILES
     This program is simply a shell script.  Users are encouraged
     to	make their own shell scripts using similar techniques.
     See $GISBASE/scripts/blend.sh.

SEE ALSO
     r.colors, r.mapcalc

AUTHOR
     Dave Gerdes, U.S. Army Construction Engineering Research
     Laboratory














GRASS 4.2		Baylor University			2



