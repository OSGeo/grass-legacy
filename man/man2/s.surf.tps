.TH s.surf.tps
.SH NAME
\fIs.surf.tps\fR - Interpolates and computes topographic analysis from 
given site data to GRASS raster format using spline with tension.
.br
\fI(GRASS Raster Program)\fR
.SH SYNOPSIS
.B s.surf.tps
.br
\fBs.surf.tps help\fR
.br
\fBs.surf.tps \fR [\fB-h\fR] \fBinput = name\fR \fBelev = name\fR
 [\fBslope = name\fR] [\fBaspect = name\fR]   
 [\fBpcurv = name\fR] [\fBtcurv = name\fR] [\fBmcurv = name\fR] 
 [\fBmaskmap= name\fR] [\fBdmin1 = val\fR] [\fBzmult = val\fR] 
 [\fBtension = val\fR] [\fBsmooth = val\fR]
 [\fBsegmax = val\fR] [\fBnpmin = val\fR]
.SH DESCRIPTION
\fIs.surf.tps\fR
.br
This program interpolates the values to grid cells from point data (digitized
contours, climatic stations, drill holes, etc.) given in a sites 
file named \fIinput\fR.  The output raster file
is \fIelev\fR.
As an option, simultaneously with interpolation,
topographic parameters slope, aspect, profile curvature (measured in the
direction of steepest slope), tangential curvature (measured in the direction
of a tangent to contour line) or mean curvature are computed and 
saved as raster files as specified by the 
options \fIslope, aspect, pcurv, tcurv, mcurv\fR respectively.
.LP
User can define a raster file named \fImaskmap\fR,
which will be used as a mask. The interpolation
is skipped for cells which have zero value in mask. Zero values 
will be assigned to these cells in all output raster files. 
.LP
Data points are checked for identical points and points that are
closer to each other, then the given \fIdmin1\fR are removed (this is necessary
especially for digitized contours).
Parameter \fIzmult\fR allows the user to rescale the z-values for sites
(useful, e.g., for transformation of elevations given in feet to meters,
so that the proper values of slopes and curvatures can be computed).
.LP
Regularized spline with tension is used for the interpolation.
The \fItension\fR parameter tunes the character of the resulting surface
from thin plate to membrane.  Higher values of tension parameter 
reduce the overshoots that can appear in surfaces with
rapid change of gradient (see suggested values for different types
of surfaces given in notes).
For noisy data, it is possible to define a smoothing parameter, \fIsmooth\fR.
With the smoothing parameter set to zero (\fIsmooth=0\fR), the resulting
surface passes exactly through the data points. 
.LP
If the number of given points is
greater than 400, segmented processing is used.
The region is split into rectangular segments, each having less than
\fIsegmax\fR points  and interpolation is performed on each
segment of the region. To ensure the smooth connection of segments
the interpolation function for each segment is computed using the points
in a given segment and the points in its neighborhood. The minimum
number of points taken for interpolation is controlled by 
\fInpmin\fR, the value of which must be larger than \fIsegmax\fR and less
than 400.  This limit of 400 was selected
to ensure the numerical stability and efficiency of the algorithm.
The program writes important values related to the computation
to the history file of raster map \fIelev\fR.

.SH OPTIONS
The user can run this program either interactively or non-interactively.
The program will be run non-interactively if the user specifies
program arguments and flag settings on the command line using the form:
.LP
.RS
\fBs.surf.tps \fR [\fB-h\fR] \fBinput = name\fR \fBelev = name\fR
 [\fBslope = name\fR] [\fBaspect = name\fR]   
 [\fBpcurv = name\fR] [\fBtcurv = name\fR] [\fBmcurv = name\fR] 
 [\fBmaskmap= name\fR] [\fBdmin1 = val\fR] [\fBzmult = val\fR] 
 [\fBtension = val\fR] [\fBsmooth = val\fR]
 [\fBsegmax = val\fR] [\fBnpmin = val\fR]

.RE
.LP
Alternatively, the user can simply type \fBs.surf.tps\fR on the command line
without program arguments.  In this case, the user will be prompted for
parameter values and flag settings using the standard GRASS parser interface
described in the manual entry for \fIparser\fR.

.LP
\fBFlags:\fR
.IP \fB-h\fR 23 
Print out the reference information.
.LP
\fBParameters:\fR
.IP \fBinput\*=\fIname\fR 18 
Use the existing site file \fIname\fR as input.
.IP \fBelev\*=\fIname\fR 18 
Output elevation values to raster file named  \fIname\fR.
.IP \fBslope\*=\fIname\fR 18 
Output slope values to raster file named  \fIname\fR.
.IP \fBaspect\*=\fIname\fR 18 
Output aspect values to raster file named  \fIname\fR.
.IP \fBpcurv\*=\fIname\fR 18 
Output profile curvature values to raster file named  \fIname\fR.
.IP \fBtcurv\*=\fIname\fR 18 
Output tangential curvature values to raster file named  \fIname\fR.
.IP \fBmcurv\*=\fIname\fR 18 
Output mean curvature values to raster file named  \fIname\fR.
.IP \fBmaskmap\*=\fIname\fR 18
Use the existing raster file \fIname\fR as a mask.
.IP \fBdmin1\*=\fIval\fR 18 
Set min distance between points to \fIval\fR.
Default value is set to 0.5 grid cell size.
.IP \fBzmult\*=\fIval\fR 18 
Convert z-values using conversion factor \fIval\fR.
Default value is 1.
.IP \fBtension\*=\fIval\fR 18 
Set tension to \fIval\fR .
Default value is 40, appropriate for smooth surfaces.
.IP \fBsmooth\*=\fIval\fR 18
Set smoothing parameter to \fIval\fR .
Default value is 0, no smoothing is performed.
.IP \fBsegmax\*=\fIval\fR 18 
Set max number of points per segment to \fIval\fR.
Default value is 40.
.IP \fBnpmin\*=\fIval\fR 18 
Set min number of points for interpolation to \fIval\fR.
Default value is 150, for data with heterogeneous spatial
distribution higher value is suggested (see Notes).

.SH NOTES
.I s.surf.tps
uses regularized spline with tension for interpolation from point
data (as described in Mitasova and Mitas, in press). The implementation
has an improved segmentation procedure based on quadtrees 
which enhances the efficiency for large data sets.
Special color tables are created by the program for output raster 
files.
.LP
Topographic parameters are computed directly from the interpolation 
function so that the important relationships between these parameters 
are preserved.
The equations for computation of these parameters and their interpretation
is described in Mitasova and Hofierka, in press.
Slopes and aspect are computed in degrees (0-90 and 1-360 respectively).
The aspect raster file has value 0 assigned to flat areas
(with slope less than 0.1%) and to singular points
with undefined aspect. Aspect points downslope and is 90 to the North,
180 to the West, 270 to the South and 360 to the East,
the values increase counterclockwise.
Curvatures are positive for convex and negative
for concave areas.
Original values of curvatures are multiplied by 100000, to conform with
GRASS integer raster files. Therefore any curvature lower than
0.00001 will be zero.  Flat areas have zero curvatures and singular points
have codes  1000000, 2000000, 3000000 for peak, pit and saddle respectively.
We suggest to use these codes only to distinguish areas (grid cells) with
undefined curvature because the codes are assigned using the theorems from
differential geometry but have never been tested.
.LP
The program gives warning when significant overshoots appear and higher
tension should be used.
However, with tension too high the resulting surface changes its behavior to
membrane (rubber sheet stretched over the data points resulting
in a peak or pit in each given point and everywhere else the surface goes 
rapidly to trend). Smoothing can also be used to reduce the overshoots
if the resulting surface should be smooth.
.LP
For data with values changing over several magnitudes
(sometimes the concentration or density data) it is suggested to interpolate the log
of the values rather than the original ones.
.LP
The program checks the numerical stability of the algorithm by computation
of values in given points, and prints the maximum difference found into the history
file of raster map \fIelev\fR. Significant increase in tension is suggested
if  the difference is unacceptable. For computation with smoothing set to 0
this difference should be 0.  With smoothing parameter greater than zero the
surface will not pass through the data points and the higher the parameter the
closer the surface will be to the trend. The maximum difference between the given and 
approximated value in this case reflects the smoothing effect on data.
 For theory on smoothing with splines and their statistical interpretation
see Talmi and Gilat 1977,  Wahba 1990, and Hutchinson 1992, where you
can find also a comparison of smoothing splines with kriging.
.LP
The program writes the values of parameters used in computation into the
comment part of the history file \fIelev\fR as well as the following values
which help to evaluate the results and choose the suitable parameters:
minimum and maximum z values in the data file (zmin_data, zmax_data)
and in the interpolated raster map (zmin_int, zmax_int), maximum
difference between the given and interpolated z value in a given point
(errtotal), rescaling parameter used for normalization (dnorm), which 
influences the tension (see Mitasova, 1992; Mitasova and Mitas, in press).

.LP
If a visible connection of segments appears, the program should be rerun with
higher \fInpmin\fR to get more points from the neighborhood of the given segment.
.LP
If the number of points in a site file is less then 400, 
\fIsegmax\fR should be set to 400 so that segmentation is not performed 
when it is not necessary.
.LP
The program gives a warning when the user wants to interpolate outside the
rectangle given by the minimum and maximum coordinates in the site file, 
zooming into
the area where the points are is suggested in this case.
.LP
When a mask is used, the program takes all points in the given region
for interpolation, including those in the area which is masked out, to ensure
proper interpolation along the border of the mask. It therefore does
not mask out the data points; if this is desirable, it must be done
outside \fIs.surf.tps\fR.

The program was used for various applications with the following
parameters :
.IP
.B "interpolation of DEM from digitized contours"
.TS
center;
l l.
tension	20. - 80.
smoothing 0.01 - 1.0
segmax	40
npmin	200 - 300
(low tension was used for relatively flat terrain, high tension
was necessary for terrain with sharp changes in slope, low value of
smoothing is usually sufficient for dense and accurately digitized
contours, for less dense and not very carefully digitized contours,
higher smoothing is suggested)
.TE
.B "interpolation of precipitation from climatic stations"
.TS
center;
l l.
tension	40. - 150.
smoothing	0. - 2.
segmax	40
npmin	200
.TE
.B "interpolation of concentration of chemicals"
.TS
center;
l l.
tension	20.  -  60.
smoothing	0.5 - 5.0
.TE
.LP
The user must run \fIg.region\fR before the program to set the region for interpolation. 
.SH "SEE ALSO"
.I "v.to.sites, g.region, r.surf.contour, r.surf.idw, r.surf.idw2, r.surf.sor, s.surf.idw"
.SH "AUTHOR"
\fIOriginal version of program (in FORTRAN)\fR:
.br
Helena Mitasova,
Illinois Natural History Survey and US Army CERL, Champaign, Illinois
 Comenius University, Bratislava, Czechoslovakia,
.br
Lubos Mitas, Department of Physics, University of Illinois at Urbana
Champaign, Illinois 
Institute of Physics, Bratislava, Czechoslovakia
.LP
\fIModified program (translated to C, adapted for GRASS , segmentation procedure)\fR:
.br
Irina Kosinovsky, US Army CERL
.br
Dave Gerdes, US Army CERL
.br
.SH "REFERENCES"
.LP
Hutchinson, M. K. and  Gessler, P. E., 1992. Splines: More than just a smooth
interpolator, \fIGeoderma\fR.
.br
.LP
Mitasova, H. and Mitas, L., in press. Interpolation by regularized spline with
tension: I. Theory and implementation, \fIMathematical Geology\fR, 25, 641-655.
.br

.LP
Mitasova, H. and Hofierka, L., in press.  Interpolation by regularized spline with
tension: II. Application to terrain modeling and surface geometry analysis,
\fIMathematical Geology\fR, 25, 657-669.
.br

.LP
Mitasova, H., 1992. New capabilities for interpolation and topographic
analysis in GRASS, \fIGRASSClippings\fR, v.6, No.2 (summer), p 13.
.br

.LP
Mitasova, H., 1992. Surfaces and modeling, \fIGRASSclippings\fR, v.6, No.3
(winter), pp 16-18.
.br

.LP
Talmi, A. and Gilat, G., 1977. Method for smooth approximation of data,
\fIJournal of Computational Physics\fR, 23, pp 93-123.
.br

.LP
Wahba, G., 1990. Spline models for observational data, CNMS-NSF Regional
Conference series in applied mathematics, 59, SIAM, Philadelphia,
Pennsylvania.

