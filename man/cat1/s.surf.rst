


s.surf.rst <main>     GRASS Reference Manual	<main> s.surf.rst




     NAME s.surf.rst - interpolation and topographic analysis
     from given site data to GRASS floating point raster format
     using regularized spline with tension (this program replaces
     s.surf.tps)

     (GRASS Site Program)


     SYNOPSIS s.surf.rst

     s.surf.rst help

     s.surf.rst [-d] [-t]&nbsp; input = name [elev = name]
     [elatt=val] [slope = name] [aspect = name] [pcurv = name]
     [tcurv = name] [mcurv = name] [maskmap = name] [dmin = val]
     [zmult = val] [tension = val] [smooth = val] [smatt=val]
     [segmax = val] [npmin = val] [devi = name] [treefile = name]
     [overfile = name]

     &nbsp;


     DESCRIPTION s.surf.rst

     This program interpolates the zi-values from point data
     (e.g., elevations, climatic stations, drill holes, etc.)
     given in a sites file named input as x|y|%z1 %z2...  to grid
     cells in the output raster file elev representing a surface.
     The user can select which floating point attribute will be
     interpolated by setting the parameter elatt to the value i
     for the i-th floating point attribute.  As an option,
     simultaneously with interpolation, topographic parameters
     slope, aspect, profile curvature (measured in the direction
     of steepest slope), tangential curvature (measured in the
     direction of a tangent to contour line) or mean curvature
     are computed and saved as raster files specified by the
     options slope, aspect, pcurv, tcurv, mcurv respectively.  If
     -d flag is set the program outputs partial derivatives fx,
     fy instead of slope, aspect, profile, tangential and mean
     curvatures respectively.  User can define a raster file
     named maskmap, which will be used as a mask. The
     interpolation is skipped for cells which have zero or NULL
     value in mask. Data points are checked for identical points
     and the points that are closer to each other than the given
     dmin are removed. Parameter zmult allows user to rescale the
     z-values for sites (useful e.g. for transformation of
     elevations given in feet to meters, so that the proper
     values of slopes and curvatures can be computed).
     Regularized spline with tension and smoothing is used
     for&nbsp; interpolation and approximation. The tension
     parameter tunes the character of the resulting surface from



GRASS 5.0beta6	      GRASS Development Team			1






s.surf.rst <main>     GRASS Reference Manual	<main> s.surf.rst



     thin plate to membrane. The flag -t can be set to use "dnorm
     independent tension", (see notes for more details about the
     tension behavior). For noisy data, it is possible to
     define&nbsp; either a constant smoothing parameter smooth or
     a variable smoothing parameter by&nbsp; setting the
     parameter smatt to the value j for the j-th floating point
     attribute in the input site file, representing the smoothing
     parameter for each point. When smoothing is used, it is
     possible to output site file devi containing deviations of
     the resulting surface from the given data.	 If the number of
     given points is greater than segmax, segmented processing is
     used. The region is split into rectangular segments, each
     having less than segmax points and interpolation is
     performed on each segment of the region. To ensure smooth
     connection of segments the interpolation function for each
     segment is computed using the points in given segment and
     the points in its neighborhood which are in the rectangular
     window surrounding the given segment. The number of points
     taken for interpolation is controlled by npmin, the value of
     which must be larger than segmax.	User can choose to output
     vector files treefile and overfile which represent the quad
     tree used for segmentation and overlapping neighborhoods
     from which additional points for interpolation on each
     segment were taken.  The program writes several important
     values to history file of raster map elev.	 If the input
     data have time stamp, the program creates time stamp for all
     output files.  The user must run g.region before the program
     to set the region and resolution for interpolation.

     &nbsp;


     OPTIONS The user can run this program either interactively
     or non-interactively.  The program will be run non-
     interactively if the user specifies program arguments and
     flag settings on the command line using the form: s.surf.rst
     [-d] [-t] input = name elev = name [elatt=val] [ slope =
     name] [ aspect = name] [ pcurv = name] [ tcurv = name] [
     mcurv = name] [ maskmap = name] [ dmin = val] [ zmult = val]
     [ tension = val] [ smooth = val] [smatt=val] [ segmax = val]
     [ npmin = val] [ devi = name] [ treefile = name] [ overfile
     = name] Alternately, the user can simply type s.surf.rst on
     the command line without program arguments. In this case,
     the user will be prompted for parameter values and flag
     settings using the standard GRASS parser interface described
     in the manual entry for parser.  Flags -d Output partial
     derivatives instead of aspect, slope and curvatures.

     -t&nbsp; Use dnorm independent tension Parameters: input =
     name

     Use the existing site file name as input.	elev = name



2		      GRASS Development Team	   GRASS 5.0beta6






s.surf.rst <main>     GRASS Reference Manual	<main> s.surf.rst



     Output elevation values to raster file name.  elatt=val

     order of floating point site attribute to use for
     interpolation (1=first), options (1-100), default is 1.
     slope = name

     Output slope or dx values to raster file name.  aspect =
     name

     Output aspect or dy values to raster file name.  pcurv =
     name

     Output profile curvature or dxx values to raster file name.
     tcurv = name

     Output tangential curvature or dyy values to raster file
     name.  mcurv = name

     Output mean curvature or dxy values to raster file name.
     maskmap = name

     Use the existing raster file name as a mask.  dmin = val

     Set min distance between points to val. Default value is set
     to 0.5 grid cell size.  zmult = val

     Convert z-values using conversion factor val. Default value
     is 1.  tension =val

     Set tension to val. Default value is 40.  smooth = val

     Set smoothing parameter to val. Default value is 0.1.
     smatt=val

     order of floating point attribute to use for variable
     smoothing parameter segmax = val

     Set max number of points per segment to val. Default value
     is 40.  npmin = val

     Set min number of points for interpolation to val. Default
     value is 200, for data with heterogeneous spatial
     distribution higher value is suggested (see notes).  devi =
     name

     Output deviations to a site file name.  treefile = name

     Output quad tree used for segmentation to vector file name
     overfile = name

     Output overlapping neighborhoods used for segmentation to
     vector file name.



GRASS 5.0beta6	      GRASS Development Team			3






s.surf.rst <main>     GRASS Reference Manual	<main> s.surf.rst



     &nbsp;


     NOTES s.surf.rst uses regularized spline with tensionfor
     interpolation from point data. Point data should be in a new
     site format %z2..., instead of the old format which confused
     categories with values (x|y|#z).  If program detects the old
     format it will allow users to have the site file rewritten
     to a new format automatically.  The implementation has a
     segmentation procedure based on quadtrees which enhances the
     efficiency for large data sets. The GRASS5.0 version has
     enhanced segmentation which takes more points for the large
     segments, to reduce the potential for visibility of
     segmentens in areas with sparse data.  Special color tables
     are created by the program for output raster files.
     Topographic parameters are computed directly from the
     interpolation function so that the important relationships
     between these parameters are preserved. The equations for
     computation of these parameters and their interpretation are
     described in (Mitasova and Hofierka 1993). Slopes and aspect
     are computed in degrees (0-90 and 1-360 respectively). The
     aspect raster file has value 0 assigned to flat areas (with
     slope less than 0.1%) and to singular points with undefined
     aspect. Aspect points downslope and is 90 to the North, 180
     to the West, 270 to the South and 360 to the East, the
     values increase counterclockwise.	Curvatures are positive
     for convex and negative for concave areas. Singular points
     with undefined curvatures have assigned zero values.
     Tension and smoothing allow user to tune the surface
     character.	 For most landscape scale applications the
     default should work fine.The program gives warning when
     significant overshoots appear in the resulting surface and
     higher tension or smoothing should be used.

     While it is possible to automatize the selection of suitable
     tension and smoothing, it has not been done yet, so here are
     some hints which may help to choose the proper parameters if
     the results look "weird".	It is useful to know that the
     method is scale dependent and the tension works as a
     rescaling parameter (high tension "increases the distances
     between the points" and reduces the range of impact of each
     point, low tension "decreases the distance" and the points
     influence each other over longer range). Surface with&nbsp;
     tension set too high behaves like a membrane (rubber sheet
     stretched over the data points) with peak or pit ("crater")
     in each given point and everywhere else the surface goes
     rapidly to trend. If digitized contours are used as input
     data, high tension can cause artificial waves along
     contours. Lower tension and higher smoothing is suggested
     for such a case.

     Surface with tension set too low behaves like a stiff steel



4		      GRASS Development Team	   GRASS 5.0beta6






s.surf.rst <main>     GRASS Reference Manual	<main> s.surf.rst



     plate and overshoots can appear in areas with rapid change
     of gradient and segmentation can be visible. Increase
     tension should solve the problems.	 There are two options
     how tension can be applied in relation to dnorm (dnorm
     rescales the coordinates depending on the average data
     density so that the size of segments with segmax=40 points
     is around 1 - this ensures the numerical stability of the
     computation): 1. Default (used also in s.surf.tps): the
     given tension&nbsp; is applied to normalized data
     (x/dnorm..),&nbsp; that means that the distances are
     multiplied (rescaled) by tension/dnorm. If density of points
     is changed, e.g.,&nbsp; by using higher dmin, the dnorm
     changes and tension needs to be changed too to get the same
     result.  Because the tension is applied to normalized data
     its suitable value is usually within the 10-100 range and
     does not depend on the actual scale (distances) of the
     original data (which can be km for regional applications or
     cm for field experiments).

     2. Flag -t (experimental for s.surf.rst): The given&nbsp;
     tension is applied to un-normalized data (rescaled tension =
     tension*dnorm/1000 is applied to normalized data (x/dnorm)
     and therefore&nbsp; dnorm cancels out) so here tension truly
     works as a rescaling parameter.  For regional applications
     with distances between points in km the suitable tension can
     be 0.1 or smaller, for detailed field scale analysis with
     distances in cm it can be 500 or more. To help select how
     much the data need to be rescaled the program writes dnorm
     and rescaled tension=tension*dnorm/1000 at the beginning of
     the program run. This rescaled tension should be around
     20-30.&nbsp; If it is lower or higher, the given tension
     parameter should be changed accordingly.  The default is a
     recommended choice, however for the applications where the
     user needs to change density of data and preserve the
     interpolation character the -t flag can be helpful.  For
     data with values changing over several magnitudes (sometimes
     the concentration or density data) it is suggested to
     interpolate the log of the values rather than the original
     ones.  The program checks the numerical stability of the
     algorithm by computing the values in given points. The root
     mean square deviation (rms) between interpolated and given
     values is written into the history file of raster map elev.
     For computation with smoothing set to 0. the rms should be
     0. Significant increase in tension is suggested if the rms
     is unexpectedly high for this case. With smoothing parameter
     greater than zero the surface will not pass exactly through
     the data points and the higher the parameter the closer the
     surface will be to the trend. The rms then represents a
     measure of smoothing effect on data. More detailed analysis
     of smoothing effects can be performed using the output
     deviations option and running s.univar on the site file with
     deviations.  The program writes the values of parameters



GRASS 5.0beta6	      GRASS Development Team			5






s.surf.rst <main>     GRASS Reference Manual	<main> s.surf.rst



     used in computation into the comment part of history file
     elev as well as the following values which help to evaluate
     the results and choose the suitable parameters: minimum and
     maximum z values in the data file (zmin_data, zmax_data) and
     in the interpolated raster map (zmin_int, zmax_int),
     rescaling parameter used for normalization (dnorm), which
     influences the tension.  When the number of points in a site
     file is not too large (less than 800), the user can skip
     segmentation by setting segmax to the number of data points
     or segmax=700.  The program gives warning when user wants to
     interpolate outside the rectangle given by minimum and
     maximum coordinates in site file, zoom into the area where
     the points are is suggested in this case.	When a mask is
     used, the program takes all points in the given region for
     interpolation, including those in the area which is masked
     out, to ensure proper interpolation along the border of the
     mask. It therefore does not mask out the data points, if
     this is desirable, it must be done outside s.surf.rst (e.g.
     using r.mask.points).  For examples of applications see
     http://www.cecer.army.mil/grass/viz/VIZ.html

     andhttp://www2.gis.uiuc.edu:2280/modviz/

     &nbsp;


     SEE ALSO r.surf.idw, r.surf.idw2, r.surf.contour,
     s.surf.idw, v.to.sites, g.region, r.mask, s.surf.tps,
     v.surf.rst, r.resamp.rst

     &nbsp;


     AUTHORS Original version of program (in FORTRAN) and GRASS
     enhancements:

     Lubos Mitas, NCSA, University of Illinois at Urbana-
     Champaign, Illinois, USA

     Helena Mitasova, Department of Geography, University of
     Illinois at Urbana-Champaign, Champaign, Illinois, USA
     Modified program (translated to C, adapted for GRASS, new
     segmentation procedure):

     Irina Kosinovsky, US Army CERL, Champaign, Illinois, USA

     Dave Gerdes, US Army CERL, Champaign, Illinois, USA
     Modifications for new sites format and timestamping:

     Darrel McCauley, Purdue University, West Laffayette,
     Indiana, USA




6		      GRASS Development Team	   GRASS 5.0beta6






s.surf.rst <main>     GRASS Reference Manual	<main> s.surf.rst



     &nbsp;

     &nbsp;


     REFERENCES Mitas, L., Mitasova, H., 1999, Spatial
     Interpolation. In: P.Longley, M.F.	 Goodchild, D.J. Maguire,
     D.W.Rhind (Eds.), Geographical Information Systems:
     Principles, Techniques, Management and Applications, Wiley,
     pp.481-492 Mitasova H., Mitas L.,&nbsp; Brown W.M.,&nbsp;
     D.P. Gerdes, I. Kosinovsky, Baker, T.1995, Modeling
     spatially and temporally distributed phenomena: New methods
     and tools for GRASS GIS. International Journal of GIS, 9
     (4), special issue on Integrating GIS and Environmental
     modeling, 433-446.	 Mitasova H. and Mitas L. 1993:
     Interpolation by Regularized Spline with Tension: I. Theory
     and Implementation, Mathematical Geology 25, 641-655.
     Mitasova H. and Hofierka L. 1993: Interpolation by
     Regularized Spline with Tension: II. Application to Terrain
     Modeling and Surface Geometry Analysis, Mathematical Geology
     25, 657-667.  Mitasova, H., 1992 : New capabilities for
     interpolation and topographic analysis in GRASS,
     GRASSclippings 6, No.2 (summer), p.13.  Mitas, L., Mitasova
     H., 1988 : General variational approach to the interpolation
     problem, Computers and Mathematics with Applications 16, p.
     983 Talmi, A. and Gilat, G., 1977 : Method for Smooth
     Approximation of Data, Journal of Computational Physics, 23,
     p.93-123.	Wahba, G., 1990, : Spline Models for
     Observational Data, CNMS-NSF Regional Conference series in
     applied mathematics, 59, SIAM, Philadelphia, Pennsylvania.

     &nbsp; Updated November 14, 1999 by Helena Mitasova























GRASS 5.0beta6	      GRASS Development Team			7



