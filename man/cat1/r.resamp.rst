


r.resamp.rst <main>   GRASS Reference Manual  <main> r.resamp.rst




     NAME r.resamp.rst - reinterpolates and computes topographic
     analysis from input raster file to a new raster file
     (possibly with different resolution) using regularized
     spline with tension and smoothing.

     (GRASS Raster Program)

     &nbsp;


     SYNOPSIS r.resamp.rst

     r.resamp.rst help

     r.resamp.rst [-r] [-d] [-t] input = name ew_res = val ns_res
     = val elev = name [slope = name] [aspect = name] [pcurv =
     name] [tcurv = name] [mcurv = name] [smooth = name] [maskmap
     = name] [overlap = val] [zmult = val] [tension = val]

     &nbsp;


     DESCRIPTION r.resamp.tps

     This program reinterpolates the values a from given raster
     file named input to a new raster file named elev. If -r flag
     is specified, all zero elevations in input file are treated
     as elevations, otherwise they are ignored. Reinterpolation
     (resampling) is done to higher, same or lower resolution
     which is specified by parameters ew_res and ns_res. All
     resulting raster files are created for the given region
     (which might be different from the header of the input
     raster file).  As an option, simultaneously with
     interpolation, topographic parameters slope, aspect, profile
     curvature (measured in the direction of steepest slope),
     tangential curvature (measured in the direction of a tangent
     to contour line) or mean curvature are computed and saved as
     raster files as specified by the options slope, aspect,
     pcurv, tcurv, mcurv respectively.	If -d flag is set the
     program outputs partial derivatives fx,fy,fxx, fxy,fyy
     instead of slope, aspect and curvatures.  For noisy data, it
     is possible to define spatially variable smoothing by
     prividing a raster file smooth containing smoothing
     parameters.  With the smoothing parameter set to zero
     (smooth is not given or contains zero data), the resulting
     surface passes exactly through the data points. User can
     define a raster file named maskmap, which will be used as a
     mask. The interpolation is skipped for cells which have zero
     value in mask. Zero values will be assigned to these cells
     in all output raster files. Parameter zmult allows the user
     to rescale the z-values (useful, e.g., for transformation of



GRASS 5.0beta6	      GRASS Development Team			1






r.resamp.rst <main>   GRASS Reference Manual  <main> r.resamp.rst



     elevations given in feet to meters, so that the proper
     values of slopes and curvatures can be computed).
     Regularized spline with tension is used for the
     interpolation. The tension parameter tunes the character of
     the resulting surface from thin plate to membrane. Higher
     values of tension parameter reduce the overshoots that can
     appear in surfaces with rapid change of gradient. The flag
     -t can be set to use "dnorm independent tension". The
     interpolation is performed for overlaping rectangular
     segments. The user can define the width of overlap (in
     number of cells) by option overlap.

     &nbsp;


     OPTIONS The user can run this program either interactively
     or non-interactively.  The program will be run non-
     interactively if the user specifies program arguments and
     flag settings on the command line using the form:
     r.resamp.rst [-r] [-d] [-t] input = name ew_res = val ns_res
     = val elev = name [slope = name] [aspect = name] [pcurv =
     name] [tcurv = name] [mcurv = name] [smooth = name] [maskmap
     = name] [overlap = val] [ zmult = val ] [tension = val]
     Alternatively, the user can simply type&nbsp; r.resamp.rst
     on the command line without program arguments. In this case,
     the user will be prompted for parameter values and flag
     settings using the standard GRASS parser interface described
     in the manual entry for parser .  Flags &nbsp;-r Indicates
     that zeroes in input map represent elevation.  -d Output
     partial derivatives instead of aspect, slope and curvatures
     Parameters: &nbsp;input =name

     Use the existing site file name as input.	&nbsp;ew_res =
     val

     Set desired east-west resolution to val .	&nbsp;ns_res =
     val

     Set desired north-south resolution to val .  elev = name

     Output elevation values to raster file named name .  slope =
     name

     Output slope or fx values to raster file named name .
     aspect = name

     Output aspect or fy values to raster file named name .
     pcurv = name

     Output profile curvature or fxx values to raster file named
     name tcurv=name




2		      GRASS Development Team	   GRASS 5.0beta6






r.resamp.rst <main>   GRASS Reference Manual  <main> r.resamp.rst



     Output tangential curvature values or fyy to raster file
     named name mcurv=name

     Output mean curvature values or fxy to raster file named
     name


     smooth=name

     18 Set smoothing parameter from file name .  maskmap=name

     Use the existing raster file name as a mask.  overlap =val

     Use overlap val cells to get additional points for
     interpolation for a given segment. Default value is 3.
     &nbsp;zmult =val

     Convert z-values using conversion factor val . Default value
     is 1.  &nbsp;tension = val

     Set tension to val .

     &nbsp;


     NOTES&nbsp; r.resamp.rst uses regularized spline with
     tension for interpolation (as described in Mitasova and
     Mitas, 1993). Region is temporarily changed while writing
     output files with desired resolution. Topographic parameters
     are computed the same way as in s.surf.rst. (See also
     Mitasova and Hofierka, 1993) Raster file smooth should
     contain variable smoothing parameters that can be derived
     from errors, slope, etc. using&nbsp; r.mapcalc.  The program
     gives warning when significant overshoots appear and higher
     tension should be used. However, with tension too high the
     resulting surface changes its behavior to membrane (rubber
     sheet stretched over the data points resulting in a peak or
     pit in each given point and everywhere else the surface goes
     rapidly to trend). Smoothing can also be used to reduce the
     overshoots. When overshoots occure the resulting elev file
     will have white color in the locations of overshoots since
     the color table for the output file is the same as
     colortable for raster input file.The program checks the
     numerical stability of the algorithm by computation of
     values in given points, and prints the maximum difference
     found into the history file of raster map elev . Increase in
     tension is suggested if the difference is unacceptable. For
     computation with smoothing set to 0 this difference should
     be 0. With smoothing parameter greater than zero the surface
     will not pass through the data points and the higher the
     parameter the closer the surface will be to the trend.  The
     program writes the values of parameters used in computation



GRASS 5.0beta6	      GRASS Development Team			3






r.resamp.rst <main>   GRASS Reference Manual  <main> r.resamp.rst



     into the comment part of the history file elev as well as
     the following values which help to evaluate the results and
     choose the suitable parameters: minimum and maximum z values
     in the data file (zmin_data, zmax_data) and in the
     interpolated raster map (zmin_int, zmax_int), maximum
     difference between the given and interpolated z value in a
     given point (errtotal), rescaling parameter used for
     normalization (dnorm), which influences the tension. The
     program gives warning when the user wants to interpolate
     outside the region given by the header of the input raster
     file, zooming into the area where the points are is
     suggested in this case. When a mask is used, the program
     takes all points in the given region for interpolation,
     including those in the area which is masked out, to ensure
     proper interpolation along the border of the mask. It
     therefore does not mask out the data points; if this is
     desirable, it must be done outside r.resamp.rst .

     &nbsp;


     SEE ALSO r.resample,
      s.surf.rst

     &nbsp;


     AUTHORS Original version of program (in FORTRAN):

     Lubos Mitas, NCSA, University of Illinois at Urbana
     Champaign, Il

     Helena Mitasova, US Army CERL, Champaign, Illinois&nbsp;
     Modified program (translated to C, adapted for GRASS ,
     segmentation procedure):

     Irina Kosinovsky, US Army CERL .

     Dave Gerdes, US Army CERL .

     &nbsp;


     REFERENCES Mitas, L., Mitasova, H., 1999, Spatial
     Interpolation. In: P.Longley, M.F.	 Goodchild, D.J. Maguire,
     D.W.Rhind (Eds.), Geographical Information Systems:
     Principles, Techniques, Management and Applications, Wiley,
     481-492.  Mitasova, H. and Mitas, L., 1993. Interpolation by
     regularized spline with tension: I. Theory and
     implementation, Mathematical Geology No.25 p.641-656.
     Mitasova, H. and Hofierka, L., 1993. Interpolation by
     regularized spline with tension: II. Application to terrain



4		      GRASS Development Team	   GRASS 5.0beta6






r.resamp.rst <main>   GRASS Reference Manual  <main> r.resamp.rst



     modeling and surface geometry analysis, Mathematical Geology
     No.25 p.657-667.  &nbsp;Talmi, A. and Gilat, G., 1977.
     Method for smooth approximation of data, Journal of
     Computational Physics , 23, pp 93-123.  Wahba, G., 1990.
     Spline models for observational data, CNMS-NSF Regional
     Conference series in applied mathematics, 59, SIAM,
     Philadelphia, Pennsylvania.
















































GRASS 5.0beta6	      GRASS Development Team			5



