.TH r.resamp.rst 1 "" "" "" ""
.SH 
NAME
\*L\*Wr.resamp.rst \*O\*O- reinterpolates and computes topographic
analysis from input raster file to a new raster file (possibly with different
resolution) using regularized spline with tension and smoothing.

.br
(\*WGRASS Raster Program\*O)

.br
&nbsp;
.SH 
SYNOPSIS
\*Lr.resamp.rst\*O

.br
\*Lr.resamp.rst help\*O

.br
\*Lr.resamp.rst [-r] [-d] [-t] input\*O = name \*Lew_res \*O= val
\*Lns_res\*O = val \*Lelev\*O = name [\*Lslope\*O = name] [\*Laspect\*O
= name] [\*Lpcurv\*O = name] [\*Ltcurv\*O = name] [\*Lmcurv\*O = name]
[\*Lsmooth\*O = name] [\*Lmaskmap\*O = name] [\*Loverlap\*O = val] [\*Lzmult\*O
= val] [\*Ltension\*O = val]

.br
&nbsp;
.SH 
DESCRIPTION
\*Wr.resamp.tps\*O

.br
This program reinterpolates the values a from given raster file named
\*Winput\*O to a new raster file named \*Welev\*O. If \*W-r\*O flag
is specified, all zero elevations in \*Winput\*O file are treated as elevations,
otherwise they are ignored. Reinterpolation (resampling) is done to higher,
same or lower resolution which is specified by parameters \*Wew_res\*O
and \*Wns_res\*O. All resulting raster files are created for the given
region (which might be different from the header of the input raster file).
As an option, simultaneously with interpolation, topographic parameters
slope, aspect, profile curvature (measured in the direction of steepest
slope), tangential curvature (measured in the direction of a tangent to
contour line) or mean curvature are computed and saved as raster files
as specified by the options \*Wslope, aspect, pcurv, tcurv, mcurv\*O respectively.
If \*W-d\*O flag is set the program outputs partial derivatives fx,fy,fxx,
fxy,fyy instead of slope, aspect and curvatures.
For noisy data, it is possible to define spatially variable smoothing
by prividing a raster file \*Wsmooth\*O containing smoothing parameters.
With the smoothing parameter set to zero (\*Wsmooth\*O is not given or
contains zero data), the resulting surface passes exactly through the data
points. User can define a raster file named \*Wmaskmap\*O, which will
be used as a mask. The interpolation is skipped for cells which have zero
value in mask. Zero values will be assigned to these cells in all output
raster files. Parameter \*Wzmult\*O allows the user to rescale the z-values
(useful, e.g., for transformation of elevations given in feet to meters,
so that the proper values of slopes and curvatures can be computed).
Regularized spline with tension is used for the interpolation. The \*Wtension\*O
parameter tunes the character of the resulting surface from thin plate
to membrane. Higher values of tension parameter reduce the overshoots that
can appear in surfaces with rapid change of gradient. The flag \*W-t\*O
can be set to use "dnorm independent tension". The interpolation is performed
for overlaping rectangular segments. The user can define the width of overlap
(in number of cells) by option \*Woverlap\*O.

.br
&nbsp;
.SH 
OPTIONS
The user can run this program either interactively or non-interactively.
The program will be run non-interactively if the user specifies program
arguments and flag settings on the command line using the form:
\*Lr.resamp.rst [-r] [-d] [-t] input\*O = name \*Lew_res\*O = val
\*Lns_res\*O = val \*Lelev\*O = name [\*Lslope\*O = name] [\*Laspect\*O
= name] [\*Lpcurv\*O = name] [\*Ltcurv\*O = name] [\*Lmcurv\*O = name]
[\*Lsmooth\*O = name] [\*Lmaskmap\*O = name] [\*Loverlap\*O = val] [
\*Lzmult \*O= val ] [\*Ltension\*O = val]
Alternatively, the user can simply type&nbsp; \*Lr.resamp.rst\*O
on the command line without program arguments. In this case, the user will
be prompted for parameter values and flag settings using the standard GRASS
parser interface described in the manual entry for \*Wparser \*O.
\*LFlags\*O
&nbsp;\*L-r\*O Indicates that zeroes in input map represent elevation.
\*L-d\*O Output partial derivatives instead of aspect, slope and curvatures
\*LParameters\*O:
&nbsp;\*Linput\*O =\*Wname\*O

.br
Use the existing site file \*Wname\*O as input.
&nbsp;\*Lew_res \*O= \*Wval\*O

.br
Set desired east-west resolution to \*Wval\*O .
&nbsp;\*Lns_res \*O= \*Wval\*O

.br
Set desired north-south resolution to \*Wval\*O .
\*Lelev \*O= \*Wname\*O

.br
Output elevation values to raster file named \*Wname\*O .
\*Lslope \*O= \*Wname\*O

.br
Output slope or fx values to raster file named \*Wname\*O .
\*Laspect \*O= \*Wname\*O

.br
Output aspect or fy values to raster file named \*Wname\*O .
\*Lpcurv \*O= \*Wname\*O

.br
Output profile curvature or fxx values to raster file named \*Wname\*O
.
\*Ltcurv\*O=\*Wname\*O

.br
Output tangential curvature values or fyy to raster file named \*Wname\*O
.
\*Lmcurv\*O=\*Wname\*O

.br
Output mean curvature values or fxy to raster file named \*Wname\*O

.br
.

.br
\*Lsmooth\*O=\*Wname\*O

.br
18 Set smoothing parameter from file \*Wname\*O .
\*Lmaskmap\*O=\*Wname\*O

.br
Use the existing raster file \*Wname\*O as a mask.
\*Loverlap \*O=\*Wval\*O

.br
Use overlap \*Wval\*O cells to get additional points for interpolation
for a given segment. Default value is 3.
&nbsp;\*Lzmult \*O=\*Wval\*O

.br
Convert z-values using conversion factor \*Wval\*O . Default value
is 1.
&nbsp;\*Ltension \*O= \*Wval\*O

.br
Set tension to \*Wval\*O .

.br
&nbsp;
.SH 
NOTES&nbsp;
\*Wr.resamp.rst\*O uses regularized spline with tension for interpolation
(as described in Mitasova and Mitas, 1993). Region is temporarily changed
while writing output files with desired resolution. Topographic parameters
are computed the same way as in s.surf.rst. (See also Mitasova and Hofierka,
1993) Raster file \*Wsmooth\*O should contain variable smoothing parameters
that can be derived from errors, slope, etc. using&nbsp; \*Wr.mapcalc\*O.
The program gives warning when significant overshoots appear and higher
tension should be used. However, with tension too high the resulting surface
changes its behavior to membrane (rubber sheet stretched over the data
points resulting in a peak or pit in each given point and everywhere else
the surface goes rapidly to trend). Smoothing can also be used to reduce
the overshoots. When overshoots occure the resulting \*Welev\*O file will
have white color in the locations of overshoots since the color table for
the output file is the same as colortable for raster input file.The program
checks the numerical stability of the algorithm by computation of values
in given points, and prints the maximum difference found into the history
file of raster map \*Welev\*O . Increase in tension is suggested if the
difference is unacceptable. For computation with smoothing set to 0 this
difference should be 0. With smoothing parameter greater than zero the
surface will not pass through the data points and the higher the parameter
the closer the surface will be to the trend.
The program writes the values of parameters used in computation into
the comment part of the history file \*Welev\*O as well as the following
values which help to evaluate the results and choose the suitable parameters:
minimum and maximum z values in the data file (zmin_data, zmax_data) and
in the interpolated raster map (zmin_int, zmax_int), maximum difference
between the given and interpolated z value in a given point (errtotal),
rescaling parameter used for normalization (dnorm), which influences the
tension. The program gives warning when the user wants to interpolate outside
the region given by the header of the input raster file, zooming into the
area where the points are is suggested in this case. When a mask is used,
the program takes all points in the given region for interpolation, including
those in the area which is masked out, to ensure proper interpolation along
the border of the mask. It therefore does not mask out the data points;
if this is desirable, it must be done outside \*Wr.resamp.rst\*O .

.br
&nbsp;
.SH 
SEE ALSO
\*Lr.resample\*O,
\*L s.surf.rst\*O

.br
&nbsp;
.SH 
AUTHORS
\*WOriginal version of program (in FORTRAN):\*O

.br
Lubos Mitas, NCSA, University of Illinois at Urbana Champaign, Il

.br
Helena Mitasova, US Army CERL, Champaign, Illinois&nbsp;
\*WModified program (translated to C, adapted for GRASS , segmentation
procedure):\*O

.br
Irina Kosinovsky, US Army CERL .

.br
Dave Gerdes, US Army CERL .

.br
&nbsp;
.SH 
REFERENCES
Mitas, L., Mitasova, H., 1999, Spatial Interpolation. In: P.Longley, M.F.
Goodchild, D.J. Maguire, D.W.Rhind (Eds.), Geographical Information Systems:
Principles, Techniques, Management and Applications, Wiley, 481-492.
Mitasova, H. and Mitas, L., 1993. Interpolation by regularized spline
with tension: I. Theory and implementation, Mathematical Geology No.25
p.641-656.
Mitasova, H. and Hofierka, L., 1993. Interpolation by regularized spline
with tension: II. Application to terrain modeling and surface geometry
analysis, Mathematical Geology No.25 p.657-667.
&nbsp;Talmi, A. and Gilat, G., 1977. Method for smooth approximation
of data, Journal of Computational Physics , 23, pp 93-123.
Wahba, G., 1990. Spline models for observational data, CNMS-NSF Regional
Conference series in applied mathematics, 59, SIAM, Philadelphia, Pennsylvania.
