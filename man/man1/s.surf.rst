.TH s.surf.rst 1 "" "" "" ""
.SH 
NAME
\*L\*Ws.surf.rst\*O \*O- interpolation and topographic analysis from
given site data to GRASS floating point raster format using regularized
spline with tension (this program replaces \*Ws.surf.tps\*O)

.br
\*W(GRASS Site Program)\*O
.SH 
SYNOPSIS
\*Ls.surf.rst\*O

.br
\*Ls.surf.rst help\*O

.br
\*Ls.surf.rst [-d] [-t]  input \*O= name [\*Lelev \*O= name]
[\*Lelatt\*O=val] [\*Lslope \*O= name] [\*Laspect
\*O= name] [\*Lpcurv
\*O=
name] [\*Ltcurv \*O= name] [\*Lmcurv \*O= name] [\*Lmaskmap\*O = name]
[\*Ldmin \*O= val] [\*Lzmult = \*Oval] [\*Ltension
\*O= val] [\*Lsmooth
\*O=
val] [\*Lsmatt=\*Oval] [\*Lsegmax = \*Oval] [\*Lnpmin
\*O= val] [\*Ldevi
\*O=
name] [\*Ltreefile \*O= name] [\*Loverfile \*O= name]

.br
 
.SH 
DESCRIPTION
\*Ws.surf.rst\*O

.br
This program interpolates the zi-values
from point data (e.g., elevations, climatic stations, drill holes, etc.)
given in a sites file named \*Winput\*O as x|y|%z1
%z2...
to grid cells in the output raster file
\*Welev\*O representing a surface.
The user can select which floating point attribute will be interpolated
by setting the parameter \*Welatt\*O to the value i for the i-th floating
point attribute\*L.\*O
As an option, simultaneously with interpolation, topographic parameters
slope, aspect, profile curvature (measured in the direction of steepest
slope), tangential curvature (measured in the direction of a tangent to
contour line) or mean curvature are computed and saved as raster files
specified by the options \*Wslope, aspect, pcurv, tcurv, mcurv\*O respectively.
If \*W-d\*O flag is set the program outputs partial derivatives fx,
fy instead of
slope, aspect, profile, tangential and mean curvatures respectively.
User can define a raster file named \*Wmaskmap\*O, which will be used
as a mask. The interpolation is skipped for cells which have zero or NULL
value in mask. Data points are checked for identical points and the points
that are closer to each other than the given \*Wdmin\*O are removed. Parameter
\*Wzmult\*O
allows user to rescale the z-values for sites (useful e.g. for transformation
of elevations given in feet to meters, so that the proper values of slopes
and curvatures can be computed).
Regularized spline with tension and smoothing is used for  interpolation
and approximation. The \*Wtension\*O parameter tunes the character of
the resulting surface from thin plate to membrane. The flag \*W-t \*Ocan
be set to use "dnorm independent tension", (see notes for more details
about the tension behavior). For noisy data, it is possible to define 
either a constant smoothing parameter \*Wsmooth\*O or a variable smoothing
parameter by  setting the parameter
\*Wsmatt\*O to the value j for
the j-th floating point attribute in the\*W input\*O site file, representing
the smoothing parameter for each point. When smoothing is used, it is possible
to output site file
\*Wdevi\*O containing deviations of the resulting
surface from the given data.
If the number of given points is greater than \*Wsegmax\*O, segmented
processing is used. The region is split into rectangular segments, each
having less than
\*Wsegmax\*O points and interpolation is performed on
each segment of the region. To ensure smooth connection of segments the
interpolation function for each segment is computed using the points in
given segment and the points in its neighborhood which are in the rectangular
window surrounding the given segment. The number of points taken for interpolation
is controlled by \*Wnpmin\*O, the value of which must be larger than \*Wsegmax\*O.
User can choose to output vector files \*Wtreefile\*O and \*Woverfile\*O
which represent the quad tree used for segmentation and overlapping neighborhoods
from which additional points for interpolation on each segment were taken.
The program writes several important values to history file of raster map
\*Welev\*O.
If the input data have time stamp, the program creates time stamp for all
output files.
The user must run \*Wg.region\*O before the program to set the region
and resolution for interpolation.

.br
 
.SH 
OPTIONS
The user can run this program either interactively or non-interactively.
The program will be run non-interactively if the user specifies program
arguments and flag settings on the command line using the form:
\*Ls.surf.rst [-d] [-t] input \*O= name \*Lelev \*O= name [\*Lelatt\*O=val]
[\*L slope \*O= name] [\*L aspect \*O= name] [\*L pcurv \*O= name] [\*L
tcurv \*O= name] [\*L mcurv \*O= name] [\*L maskmap\*O = name] [\*L
dmin \*O= val] [\*L zmult = \*Oval] [\*L tension \*O= val] [\*L smooth
\*O=
val] [\*Lsmatt\*O=val] [\*L segmax = \*Oval] [\*L npmin \*O= val] [\*L
devi \*O= name] [\*L treefile \*O= name] [\*L overfile \*O= name]
Alternately, the user can simply type \*Ls.surf.rst\*O on the command
line without program arguments. In this case, the user will be prompted
for parameter values and flag settings using the standard GRASS parser
interface described in the manual entry for\*W parser\*O.
\*LFlags\*O
\*L-d\*O Output partial derivatives instead of aspect, slope and curvatures.

.br
\*L-t\*O  Use dnorm independent tension
\*LParameters:\*O
\*Linput \*O= \*Wname\*O

.br
Use the existing site file name as input.
\*Lelev \*O= \*Wname\*O

.br
Output elevation values to raster file \*Wname\*O.
\*Lelatt\*O=\*Wval\*O

.br
order of floating point site attribute to use for interpolation (1=first),
options (1-100), default is 1.
\*Lslope \*O= \*Wname\*O

.br
Output slope or dx values to raster file \*Wname\*O.
\*Laspect \*O= \*Wname\*O

.br
Output aspect or dy values to raster file \*Wname\*O.
\*Lpcurv \*O= \*Wname\*O

.br
Output profile curvature or dxx values to raster file \*Wname\*O.
\*Ltcurv\*O = \*Wname\*O

.br
Output tangential curvature or dyy values to raster file \*Wname\*O.
\*Lmcurv\*O = \*Wname\*O

.br
Output mean curvature or dxy values to raster file \*Wname\*O.
\*Lmaskmap\*O = \*Wname\*O

.br
Use the existing raster file \*Wname\*O as a mask.
\*Ldmin\*O = \*Wval\*O

.br
Set min distance between points to \*Wval\*O. Default value is set
to 0.5 grid cell size.
\*Lzmult\*O = \*Wval\*O

.br
Convert z-values using conversion factor \*Wval\*O. Default value
is 1.
\*Ltension\*O =\*Wval\*O

.br
Set tension to \*Wval\*O. Default value is 40.
\*Lsmooth\*O = \*Wval\*O

.br
Set smoothing parameter to \*Wval\*O. Default value is 0.1.
\*Lsmatt\*O=\*Wval\*O

.br
order of floating point attribute to use for variable smoothing parameter
\*Lsegmax\*O = \*Wval\*O

.br
Set max number of points per segment to \*Wval\*O. Default value is
40.
\*Lnpmin\*O = \*Wval\*O

.br
Set min number of points for interpolation to \*Wval\*O. Default value
is 200, for data with heterogeneous spatial distribution higher value is
suggested (see notes).
\*Ldevi\*O = \*Wname\*O

.br
Output deviations to a site file \*Wname\*O.
\*Ltreefile\*O = \*Wname\*O

.br
Output quad tree used for segmentation to vector file \*Wname\*O
\*Loverfile\*O = name

.br
Output overlapping neighborhoods used for segmentation to vector file
\*Wname\*O.

.br
 
.SH 
NOTES
\*Ws.surf.rst\*O\*L \*Ouses regularized spline with tensionfor interpolation
from point data. Point data should be in a \*Lnew
site format 
%z2...,
instead of the old format which confused categories with values (x|y|#z).
If program detects the old format it will allow users to have the site
file rewritten to a new format automatically.
The implementation has a segmentation procedure based on quadtrees which
enhances the efficiency for large data sets. The GRASS5.0 version has enhanced
segmentation which takes more points for the large segments, to reduce
the potential for visibility of segmentens in areas with sparse data.
Special color tables are created by the program for output raster files.
Topographic parameters are computed directly from the interpolation
function so that the important relationships between these parameters are
preserved. The equations for computation of these parameters and their
interpretation are described in \*L(Mitasova
and Hofierka 1993). Slopes and aspect are computed in degrees (0-90
and 1-360 respectively). The aspect raster file has value 0 assigned to
flat areas (with slope less than 0.1%) and to singular points with undefined
aspect. Aspect points downslope and is 90 to the North, 180 to the West,
270 to the South and 360 to the East, the values increase counterclockwise.
Curvatures are positive for convex and negative for concave areas. Singular
points with undefined curvatures have assigned zero values.
\*WTension\*O and \*Wsmooth\*Oing allow user to tune the surface character.
For most landscape scale applications the default should work fine.The
program gives warning when significant overshoots appear in the resulting
surface and higher tension or smoothing should be used.

.br
While it is possible to automatize the selection of suitable \*Wtension\*O
and \*Wsmooth\*Oing, it has not been done yet, so here are some hints
which may help to choose the proper parameters if the results look "weird".
It is useful to know that the method is scale dependent and the \*Wtension\*O
works as a rescaling parameter (high \*Wtension\*O "increases the distances
between the points" and reduces the range of impact of each point, low\*W
tension\*O "decreases the distance" and the points influence each other
over longer range). Surface with  \*Wtension\*O set too high behaves
like a membrane (rubber sheet stretched over the data points) with peak
or pit ("crater") in each given point and everywhere else the surface goes
rapidly to trend. If digitized contours are used as input data, high tension
can cause artificial waves along contours. Lower tension and higher smoothing
is suggested for such a case.

.br
Surface with \*Wtension\*O set too low behaves like a stiff steel
plate and overshoots can appear in areas with rapid change of gradient
and segmentation can be visible. Increase tension should solve the problems.
There are two options how \*Wtension\*O can be applied in relation
to \*Wdnorm\*O (dnorm rescales the coordinates depending on the average
data density so that the size of segments with \*Wsegmax=\*O40 points
is around 1 - this ensures the numerical stability of the computation):
1. Default (used also in s.surf.tps): the given \*Wtension\*O 
is applied to normalized data (x/\*Wdnorm\*O..),  that means that
the distances are multiplied (rescaled) by \*Wtension/dnorm\*O. If density
of points is changed, e.g.,  by using higher \*Wdmin\*O, the \*Wdnorm\*O
changes and \*Wtension\*O needs to be changed too to get the same result.
Because the \*Wtension\*O is applied to normalized data its suitable value
is usually within the 10-100 range and does not depend on the actual scale
(distances) of the original data (which can be km for regional applications
or cm for field experiments).

.br
2. Flag\*L -t \*O(experimental for s.surf.rst)\*L: \*OThe given 
\*Wtension\*O is applied to un-normalized data (rescaled tension = t\*Wension*dnorm\*O/1000
is applied to normalized data (x/\*Wdnorm\*O) and therefore  \*Wdnorm\*O
cancels out) so here \*Wtension\*O truly works as a rescaling parameter.
For regional applications with distances between points in km the suitable
tension can be 0.1 or smaller, for detailed field scale analysis with distances
in cm it can be 500 or more. To help select how much the data need to be rescaled 
the program writes
\*Wdnorm\*O and rescaled tension=\*Wtension*dnorm\*O/1000 at the
beginning of the program run. This rescaled \*Wtension\*O should be around
20-30.  If it is lower or higher, the given \*Wtension\*O parameter
should be changed accordingly.
The default is a recommended choice, however for the applications where
the user needs to change density of data and preserve the interpolation
character the \*L-t\*O flag can be helpful.
For data with values changing over several magnitudes (sometimes the
concentration or density data) it is suggested to interpolate the log of
the values rather than the original ones.
The program checks the numerical stability of the algorithm by computing
the values in given points. The root mean square deviation (rms) between
interpolated and given values is written into the history file of raster
map \*Welev\*O. For computation with smoothing set to 0. the rms should
be 0. Significant increase in tension is suggested if the rms is unexpectedly
high for this case. With smoothing parameter greater than zero the surface
will not pass exactly through the data points and the higher the parameter
the closer the surface will be to the trend. The rms then represents a
measure of smoothing effect on data. More detailed analysis of smoothing
effects can be performed using the output deviations option and running
s.univar on the site file with deviations.
The program writes the values of parameters used in computation into
the comment part of history file \*Welev\*O as well as the following values
which help to evaluate the results and choose the suitable parameters:
minimum and maximum z values in the data file (zmin_data, zmax_data) and
in the interpolated raster map (zmin_int, zmax_int), rescaling parameter
used for normalization (dnorm), which influences the tension.
When the number of points in a site file is not too large (less than
800), the user can skip segmentation by setting \*Wsegmax\*O to the number
of data points or segmax=700.
The program gives warning when user wants to interpolate outside the
rectangle given by minimum and maximum coordinates in site file, zoom into
the area where the points are is suggested in this case.
When a mask is used, the program takes all points in the given region
for interpolation, including those in the area which is masked out, to
ensure proper interpolation along the border of the mask. It therefore
does not mask out the data points, if this is desirable, it must be done
outside s.surf.rst (e.g. using r.mask.points).
For examples of applications see \*Lhttp://www.cecer.army.mil/grass/viz/VIZ.html\*O

.br
and\*Lhttp://www2.gis.uiuc.edu:2280/modviz/\*O

.br
 
.SH 
SEE ALSO
\*Lr.surf.idw\*O,
\*Lr.surf.idw2\*O,
\*Lr.surf.contour\*O,
\*Ls.surf.idw\*O,
\*Lv.to.sites\*O,
\*Lg.region\*O,
\*Lr.mask\*O,
\*Ls.surf.tps\*O,
\*Lv.surf.rst\*O,
\*Lr.resamp.rst\*O

.br
 
.SH 
AUTHORS
\*WOriginal version of program (in FORTRAN) and GRASS enhancements:\*O

.br
Lubos Mitas, NCSA, University of Illinois at Urbana-Champaign, Illinois,
USA

.br
Helena Mitasova, Department of Geography, University of Illinois at
Urbana-Champaign, Champaign, Illinois, USA
\*WModified program (translated to C, adapted for GRASS, new segmentation
procedure):\*O

.br
Irina Kosinovsky, US Army CERL, Champaign, Illinois, USA

.br
Dave Gerdes, US Army CERL, Champaign, Illinois, USA
\*WModifications for new sites format and timestamping:\*O

.br
Darrel McCauley, Purdue University, West Laffayette, Indiana, USA

.br
 

.br
 
.SH 
REFERENCES
Mitas, L., Mitasova, H., 1999, Spatial Interpolation. In: P.Longley, M.F.
Goodchild, D.J. Maguire, D.W.Rhind (Eds.), \*WGeographical Information
Systems: Principles, Techniques, Management and Applications\*O, Wiley,
pp.481-492
Mitasova H., Mitas L.,  Brown W.M.,  D.P. Gerdes, I. Kosinovsky,
Baker, T.1995, Modeling spatially and temporally distributed phenomena:
New methods and tools for GRASS GIS. \*WInternational Journal of GIS\*O,
9 (4), special issue on Integrating GIS and Environmental modeling, 433-446.
\*LMitasova
H. and Mitas L. 1993: Interpolation by Regularized Spline with Tension:
I. Theory and Implementation, \*WMathematical Geology\*O 25, 641-655.
\*LMitasova
H. and Hofierka L. 1993: Interpolation by Regularized Spline with Tension:
II. Application to Terrain Modeling and Surface Geometry Analysis, \*WMathematical
Geology\*O 25, 657-667.
Mitasova, H., 1992 : New capabilities for interpolation and topographic
analysis in GRASS, \*WGRASSclippings \*O6, No.2 (summer), p.13.
Mitas, L., Mitasova H., 1988 : General variational approach to the interpolation
problem, \*WComputers and Mathematics with Applications \*O16, p. 983
Talmi, A. and Gilat, G., 1977 : Method for Smooth Approximation of Data,
\*WJournal
of Computational Physics\*O, 23, p.93-123.
Wahba, G., 1990, : Spline Models for Observational Data, CNMS-NSF Regional
Conference series in applied mathematics, 59, SIAM, Philadelphia, Pennsylvania.

.br
 
Updated November 14, 1999 by Helena Mitasova
