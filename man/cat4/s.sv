


s.sv <contrib>	      GRASS Reference Manual	   <contrib> s.sv



NAME
     s.sv - Sample semivariogram of a GRASS sites list.
     (GRASS Sites Program)

SYNOPSIS
     s.sv
     s.sv help
     s.sv [-alq] [sites=name] lag=value [lagtol=value]
     [direction=value] [angtol=value] [graph=name]

DESCRIPTION
     s.sv calculates a sample semivariogram and either plots it
     or writes it to standard output.

     For more information, refer to the tutorial or see the
     example below.

OPTIONS
     Flags:

     -q		       Quiet. Cut out the chatter.

     -p		       Plot the sample semivariogram in the GRASS
		       graphics window (requires g.gnuplot).

     Parameters:

     sites=name	       Name of an existing sites file with
		       floating-point attributes (the variable
		       for which we are calculating
		       semivariance).  Default is standard input
		       with no field separators.

     lag=value	       Nominal lag distance.

     lagtol=value      Tolerance on lag distance. Default is half
		       of nominal distance.

     direction=value   Direction of semivariogram. Default is
		       omnidirectional semivariogram.

     angtol=value      Angular tolerance on direction.

     graph=name	       Basename to save graphing data/commands
		       files.  Graphs are saved in the current
		       working directory with the extensions .gp
		       and .dat. Implies the -p flag. If
		       unspecified, semivariogram is written to
		       standard output.

NOTES
     Without the -p flag, three columns of data are written to



GRASS 4.2.1		Baylor University			1






s.sv <contrib>	      GRASS Reference Manual	   <contrib> s.sv



     standard output: lag distance (h), semivariogram value
     (gamma), and the number of data pairs used to compute it
     (N(h)). When the graph parameter is set, these same three
     columns of data are written to name.dat. Therefore, to
     replot a sample semivariogram, use:

	  g.gnuplot name.gp

     To plot a histogram of N(h), simply edit name.gp and redo
     the previously given command.

SEE ALSO
     s.univar, s.normal, g.gnuplot, m.svfit and
     Semivariogram Modeling - A GRASS Tutorial on Exploratory
     Data Analysis and Semivariogram Modeling.

BUGS
     Will not work correctly with lat-long data.  Should
     G_azimuth() be used to calculate the angle between points?

     Only Matheron's classical estimator is available with s.sv.
     Others may be added in the future.

     Please send all bug fixes and comments to the author.

AUTHOR
     James Darrell McCauley, Agricultural Engineering, Purdue
     University
     (mccauley@ecn.purdue.edu)

NOTICE
     This program is part of the contrib section of the GRASS
     distribution.  As such, it is externally contributed code
     that has not been examined or tested by the Office of GRASS
     Integration.




















GRASS 4.2.1		Baylor University			2



