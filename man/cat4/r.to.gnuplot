


r.to.gnuplot <contrib>GRASS Reference Manua<contrib> r.to.gnuplot



NAME
     r.to.gnuplot - outputs a raster map in GNUPLOT format
     (GRASS Shell Script)

SYNOPSIS
     r.to.gnuplot help
     r.to.gnuplot name

DESCRIPTION
     r.to.gnuplot is a Bourne shell script that converts a raster
     map into a format suitable for plotting with g.gnuplot and
     writes the results to standard output.

OPTIONS
     This program runs non-interactively;  the user must state
     all parameter values on the command line.

     Parameter:

     name	       Name of a raster map layer.

EXAMPLE
     Typing the following at the command line:

	  r.to.gnuplot elevation > elev.dat

     will write the raster data to elev.dat.  After staring the
     GRASS graphics monitor, the following dialogue:

	  g.gnuplot
	  gnuplot> set parametric
	  gnuplot> set contour base
	  gnuplot> set nosurface
	  gnuplot> set view 180,0
	  gnuplot> splot 'elev.dat' notitle with lines

     will plot a contour map of elevation.

NOTES
     Similar procedures may be used to plot wire-mesh surfaces.

     g.gnuplot may be used to simultaneously plot surfaces and
     contours from multiple raster maps.

     Output may be saved as PostScript, FrameMaker, TeX, etc
     (approximately 2 dozen output formats).

FILES
     $GISBASE/scripts/r.to.gnuplot.

SEE ALSO
     r.stats, v.to.gnuplot, and g.gnuplot



GRASS 5.0beta2	      GRASS Development Team			1






r.to.gnuplot <contrib>GRASS Reference Manua<contrib> r.to.gnuplot



AUTHOR
     James Darrell McCauley, Agricultural Engineering, Purdue
     University

NOTICE
     This program is part of the contrib section of the GRASS
     distribution.  As such, it is externally contributed code
     that has not been examined or tested by the Office of GRASS
     Integration.














































2		      GRASS Development Team	   GRASS 5.0beta2



