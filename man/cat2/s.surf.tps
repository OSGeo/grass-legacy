


s.surf.tps <alpha>    GRASS Reference Manual   <alpha> s.surf.tps




     NAME s.surf.tps - Interpolates and computes topographic
     analysis from given site data to GRASS raster format using
     spline with tension (integer version from GRASS4.1, use
     s.surf.rst for floating point in GRASS5.0).

     (GRASS Raster Program)


     SYNOPSIS s.surf.tps

     s.surf.tps help

     s.surf.tps input = name elev = name [slope = name] [aspect =
     name] [pcurv = name] [tcurv = name] [mcurv = name] [maskmap=
     name] [dmin1 = val] [zmult = val] [tension = val] [smooth =
     val] [segmax = val] [npmin = val]


     DESCRIPTION s.surf.tps

     This program interpolates the values to grid cells from
     point data (digitized contours, climatic stations, drill
     holes, etc.) given in a sites file named input. The output
     raster file is elev. As an option, simultaneously with
     interpolation, topographic parameters slope, aspect, profile
     curvature (measured in the direction of steepest slope),
     tangential curvature (measured in the direction of a tangent
     to contour line) or mean curvature are computed and saved as
     raster files as specified by the options slope, aspect,
     pcurv, tcurv, mcurv respectively.	User can define a raster
     file named maskmap, which will be used as a mask. The
     interpolation is skipped for cells which have zero value in
     mask. Zero values will be assigned to these cells in all
     output raster files.  Data points are checked for identical
     points and points that are closer to each other, then the
     given dmin1 are removed (this is necessary especially for
     digitized contours). Parameter zmult allows the user to
     rescale the z-values for sites (useful, e.g., for
     transformation of elevations given in feet to meters, so
     that the proper values of slopes and curvatures can be
     computed).	 Regularized spline with tension is used for the
     interpolation. The tension parameter tunes the character of
     the resulting surface from thin plate to membrane. Higher
     values of tension parameter reduce the overshoots that can
     appear in surfaces with rapid change of gradient (see
     suggested values for different types of surfaces given in
     notes). For noisy data, it is possible to define a smoothing
     parameter, smooth. With the smoothing parameter set to zero
     (smooth=0), the resulting surface passes exactly through the
     data points.  If the number of given points is greater than
     400, segmented processing is used. The region is split into



GRASS 5.0beta3	      GRASS Development Team			1






s.surf.tps <alpha>    GRASS Reference Manual   <alpha> s.surf.tps



     rectangular segments, each having less than segmax points
     and interpolation is performed on each segment of the
     region. To ensure the smooth connection of segments the
     interpolation function for each segment is computed using
     the points in a given segment and the points in its
     neighborhood. The minimum number of points taken for
     interpolation is controlled by npmin, the value of which
     must be larger than segmax and less than 400. This limit of
     400 was selected to ensure the numerical stability and
     efficiency of the algorithm. The program writes important
     values related to the computation to the history file of
     raster map elev.


     OPTIONS The user can run this program either interactively
     or non-interactively.  The program will be run non-
     interactively if the user specifies program arguments and
     flag settings on the command line using the form: s.surf.tps
     input=name elev=name [slope=name] [aspect=name] [pcurv=name]
     [tcurv=name] [mcurv=name] [maskmap=name] [dmin1=val]
     [zmult=val] [tension=val] [smooth=val] [segmax=val]
     [npmin=val] Alternatively, the user can simply type
     s.surf.tps on the command line without program arguments. In
     this case, the user will be prompted for parameter values
     and flag settings using the standard GRASS parser interface
     described in the manual entry for parser.


     Parameters: Use the existing site file name as input.
     Output elevation values to raster file named name.	 Output
     slope values to raster file named name.  Output aspect
     values to raster file named name.	Output profile curvature
     values to raster file named name.	Output tangential
     curvature values to raster file named name.  Output mean
     curvature values to raster file named name.  Use the
     existing raster file name as a mask.  Set min distance
     between points to val. Default value is set to 0.5 grid cell
     size.  Convert z-values using conversion factor val. Default
     value is 1.  Set tension to val . Default value is 40,
     appropriate for smooth surfaces.  Set smoothing parameter to
     val . Default value is 0, no smoothing is performed.  Set
     max number of points per segment to val. Default value is
     40.  Set min number of points for interpolation to val.
     Default value is 150, for data with heterogeneous spatial
     distribution higher value is suggested (see Notes).

     &nbsp; &nbsp;

     &nbsp;

     &nbsp;




2		      GRASS Development Team	   GRASS 5.0beta3






s.surf.tps <alpha>    GRASS Reference Manual   <alpha> s.surf.tps



     &nbsp;


     NOTES LINUX There are some peculiar differences between
     s.surf.tps for LINUX and e.g. for SUN.  - e.g. the
     interpolation of the heroldsberg elevation maps was using on
     SUN a temporary space of 170Mb, on LINUX approx. 80Mb.
     Please play with the parameters for a reasonalble result.
     s.surf.tps uses regularized spline with tension for
     interpolation from point data (as described in Mitasova and
     Mitas, in press). The implementation has an improved
     segmentation procedure based on quadtrees which enhances the
     efficiency for large data sets. Special color tables are
     created by the program for output raster files.  Topographic
     parameters are computed directly from the interpolation
     function so that the important relationships between these
     parameters are preserved. The equations for computation of
     these parameters and their interpretation is described in
     Mitasova and Hofierka, in press. Slopes and aspect are
     computed in degrees (0-90 and 1-360 respectively). The
     aspect raster file has value 0 assigned to flat areas (with
     slope less than 0.1%) and to singular points with undefined
     aspect. Aspect points downslope and is 90 to the North, 180
     to the West, 270 to the South and 360 to the East, the
     values increase counterclockwise. Curvatures are positive
     for convex and negative for concave areas. Original values
     of curvatures are multiplied by 100000, to conform with
     GRASS integer raster files. Therefore any curvature lower
     than 0.00001 will be zero. Flat areas have zero curvatures
     and singular points have codes 1000000, 2000000, 3000000 for
     peak, pit and saddle respectively.	 We suggest to use these
     codes only to distinguish areas (grid cells) with undefined
     curvature because the codes are assigned using the theorems
     from differential geometry but have never been tested.  The
     program gives warning when significant overshoots appear and
     higher tension should be used. However, with tension too
     high the resulting surface changes its behavior to membrane
     (rubber sheet stretched over the data points resulting in a
     peak or pit in each given point and everywhere else the
     surface goes rapidly to trend). Smoothing can also be used
     to reduce the overshoots if the resulting surface should be
     smooth.  For data with values changing over several
     magnitudes (sometimes the concentration or density data) it
     is suggested to interpolate the log of the values rather
     than the original ones.  The program checks the numerical
     stability of the algorithm by computation of values in given
     points, and prints the maximum difference found into the
     history file of raster map elev. Significant increase in
     tension is suggested if the difference is unacceptable. For
     computation with smoothing set to 0 this difference should
     be 0. With smoothing parameter greater than zero the surface
     will not pass through the data points and the higher the



GRASS 5.0beta3	      GRASS Development Team			3






s.surf.tps <alpha>    GRASS Reference Manual   <alpha> s.surf.tps



     parameter the closer the surface will be to the trend. The
     maximum difference between the given and approximated value
     in this case reflects the smoothing effect on data. For
     theory on smoothing with splines and their statistical
     interpretation see Talmi and Gilat 1977, Wahba 1990, and
     Hutchinson 1992, where you can find also a comparison of
     smoothing splines with kriging.  The program writes the
     values of parameters used in computation into the comment
     part of the history file elev as well as the following
     values which help to evaluate the results and choose the
     suitable parameters: minimum and maximum z values in the
     data file (zmin_data, zmax_data) and in the interpolated
     raster map (zmin_int, zmax_int), maximum difference between
     the given and interpolated z value in a given point
     (errtotal), rescaling parameter used for normalization
     (dnorm), which influences the tension (see Mitasova, 1992;
     Mitasova and Mitas, in press).  If a visible connection of
     segments appears, the program should be rerun with higher
     npmin to get more points from the neighborhood of the given
     segment.  If the number of points in a site file is less
     then 400, segmax should be set to 400 so that segmentation
     is not performed when it is not necessary.	 The program
     gives a warning when the user wants to interpolate outside
     the rectangle given by the minimum and maximum coordinates
     in the site file, zooming into the area where the points are
     is suggested in this case.	 When a mask is used, the program
     takes all points in the given region for interpolation,
     including those in the area which is masked out, to ensure
     proper interpolation along the border of the mask. It
     therefore does not mask out the data points; if this is
     desirable, it must be done outside s.surf.tps.  The program
     was used for various applications with the following
     parameters : "interpolation of DEM from digitized contours"

     smoothing 0.01 - 1.0 segmax&nbsp; 40 npmin&nbsp;&nbsp; 200 -
     300 (low tension was used for relatively flat terrain, high
     tension was necessary for terrain with sharp changes in
     slope, low value of smoothing is usually sufficient for
     dense and accurately digitized contours, for less dense and
     not very carefully digitized contours, higher smoothing is
     suggested)


     "interpolation of precipitation from climatic stations"

     smoothing&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; 0. - 2.
     segmax&nbsp; 40 npmin&nbsp;&nbsp; 200


     "interpolation of concentration of chemicals"

     smoothing&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; 0.5 - 5.0



4		      GRASS Development Team	   GRASS 5.0beta3






s.surf.tps <alpha>    GRASS Reference Manual   <alpha> s.surf.tps



     The user must run g.regionbefore the program to set the
     region for interpolation.


     SEE ALSO s.surf.rst

     v.to.sites

     g.region

     r.surf.contour

     r.surf.idw

     r.surf.idw2

     s.surf.idw


     AUTHOR Original version of program (in FORTRAN):

     Helena Mitasova, Illinois Natural History Survey and US Army
     CERL, Champaign, Illinois Comenius University, Bratislava,
     Czechoslovakia,

     Lubos Mitas, Department of Physics, University of Illinois
     at Urbana Champaign, Illinois Institute of Physics,
     Bratislava, Czechoslovakia Modified program (translated to
     C, adapted for GRASS , segmentation procedure):

     Irina Kosinovsky, US Army CERL

     Dave Gerdes, U.S.Army Construction Engineering Research
     Laboratory


     REFERENCES

     Mitasova and Mitas 1993: Interpolation by Regularized Spline
     with Tension: I. Theory and Implementation,

     Mathematical Geology ,25, 641-655.	 Mitasova and Hofierka
     1993: Interpolation by Regularized Spline with Tension: II.
     Application to Terrain Modeling

     and Surface Geometry Analysis, Mathematical Geology 25,
     657-667.  Mitas, L., Mitasova, H., 1999, Spatial
     Interpolation. In: P.Longley, M.F. Goodchild, D.J. Maguire,
     D.W.Rhind (Eds.), Geographical Information Systems:
     Principles, Techniques, Management and Applications, Wiley,
     pp.481-492 Mitasova, H., 1992. New capabilities for
     interpolation and topographic analysis in GRASS,



GRASS 5.0beta3	      GRASS Development Team			5






s.surf.tps <alpha>    GRASS Reference Manual   <alpha> s.surf.tps



     GRASSClippings, v.6, No.2 (summer), p 13.	Mitasova, H.,
     1992. Surfaces and modeling, GRASSclippings, v.6, No.3
     (winter), pp 16-18.  Talmi, A. and Gilat, G., 1977. Method
     for smooth approximation of data, Journal of Computational
     Physics, 23, pp 93-123.  Wahba, G., 1990. Spline models for
     observational data, CNMS-NSF Regional Conference series in
     applied mathematics, 59, SIAM, Philadelphia, Pennsylvania.

NOTICE
     This program is part of the alpha section of the GRASS
     distribution.  Unlike the code in the main section of GRASS,
     the alpha code has not yet been fully tested for one release
     cycle.










































6		      GRASS Development Team	   GRASS 5.0beta3



