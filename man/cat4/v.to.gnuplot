


v.to.gnuplot <contrib>GRASS Reference Manua<contrib> v.to.gnuplot



NAME
     v.to.gnuplot - outputs an ASCII vector map in GNUPLOT format
     (GRASS Shell Script)

SYNOPSIS
     v.to.gnuplot help
     v.to.gnuplot name

DESCRIPTION
     v.to.gnuplot is an awk shell script that converts an ASCII
     vector map into a format suitable for plotting with
     g.gnuplot and writes the results to standard output.

OPTIONS
     This program runs non-interactively;  the user must either
     state all parameter values on the command line or use
     redirection.

     Parameter:

     name	       Full path of an ASCII vector  map layer.

EXAMPLE
     Typing the following at the command line:

	  v.to.gnuplot < LOCATION/dig_ascii/elevation > elev.dat

     will write the raster data to elev.dat.  After staring the
     GRASS graphics monitor, the following dialogue:

	  g.gnuplot
	  gnuplot> plot 'elev.dat' notitle with lines

     will plot a map of elevation.

NOTES
     Output may be saved as PostScript, FrameMaker, TeX, etc
     (approximately 2 dozen output formats).

     v.vclean and v.out.ascii must be run prior to v.to.gnuplot.

FILES
     $GISBASE/scripts/v.to.gnuplot

SEE ALSO
     v.clean, v.out.ascii, r.to.gnuplot, and g.gnuplot

AUTHOR
     James Darrell McCauley, Agricultural Engineering, Purdue
     University





GRASS 5.0beta6	      GRASS Development Team			1






v.to.gnuplot <contrib>GRASS Reference Manua<contrib> v.to.gnuplot



NOTICE
     This program is part of the contrib section of the GRASS
     distribution.  As such, it is externally contributed code
     that has not been examined or tested by the Office of GRASS
     Integration.


















































2		      GRASS Development Team	   GRASS 5.0beta6



