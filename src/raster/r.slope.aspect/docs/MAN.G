.TH Gslope.aspect 2 G-language
.SH NAME
Gslope.aspect \- generate slope and aspect maps from elevation
.br
(G language tool)
.I (GRASS-GRID Tool)
.SH SYNOPSIS
.nf
Gslope.aspect [-v] elevation=input slope=output1 aspect=output2 [format=degrees/percent]
.fi
.SH DESCRIPTION
Gslope.aspect
generates slope and aspect maps from elevation data.
The elevation filename and at least one output filename
(slope or aspect)
must be supplied on the command line.  An elevation file in
a particular mapset may be specified using
elevation=\*(lqfilename in mapset\*(rq.  The resulting slope
and aspect maps will be created in the current mapset.
The optional verbose flag [-v] provides information
on program operations during execution.
.SH ELEVATION LAYER
The elevation layer specified by the user
must contain true elevation values,
not rescaled or categorized data.
.SH ASPECT LAYER
The aspect file which is created indicates the direction that the
slopes are facing. The aspect categories represent the number of
degrees of east .
.NF
.ne 18
  0   no data
  1   east facing
  2   15 degrees north of east
  3   30 degrees north of east
  4   northeast facing
  5   30 degrees east of north
  6   15 degrees east of north
  7   north facing
  8   15 degrees west of north
  9   30 degrees west of north
 10   northwest facing
 11   30 degrees north of west
 12   15 degrees north of west
 13   west facing
 14   15 degrees south of west
 15   30 degrees south of west
 16   southwest facing
 17   30 degrees west of south
 18   15 degrees west of south
 19   south facing
 20   15 degrees east of south
 21   30 degrees east of south
 22   southeast facing
 23   30 degrees south of east
 24   15 degrees south of east
 25   no aspect (flat)
.FI

Category and color table files are also generated for the
aspect layer.
.SH SLOPE LAYER
The resulting slope layer will contain slope in degrees of inclination
from the horizontal. Category 0 will be reserved for no data.
Category 1 will be 0 degrees slope, category 2 will be 1 degree, etc.
The slope layer could contain, in theory, up to 91 categories.
The category file is generated for the slope layer (but not the color
table).

The slope is represented in degrees by default or in percen of rise if
the option format=percent is used.

However, for most applications, it will be desired to create a reclassification
of the slopes into groups using RECLASS.
The following is an example of a useful reclassification:

.NF
.ne 10
    category     range       category labels
             (in degrees)     (in percent)
    
       1           0-1            0-2%
       2           2-3            3-5%
       3           4-5            6-10%
       4           6-8           11-15%
       5           9-11          16-20%
       6          12-14          21-25%
       7          15-90          26% and higher
.FI

Also, the following color table works well with the above reclassifcation.

.NF
.ne 10
      category      red  green  blue

         0           70%   70%   70%
         1            0%   40%    0%
         2            0%   60%    0%
         3           50%   60%    0%
         4           80%   70%    0%
         5           50%   20%   20%
         6          100%    0%    0%
         7            0%    0%    0%
.FI
.SH NOTES
The current window and mask settings are ignored.
The elevation file is read directly to insure that data is not lost
or inappropriately resampled.

The algorithm used to determine slope and aspect uses a 3x3 neighborhood
around each elevation cell.
Thus it is not possible to determine slope and aspect for the cells which
form the very edge of the elevation. These cell are assigned
\*(lqno data\*(rq
in both the slope and aspect files.

However, the algorithm does not try to determine if the values in the
elevation file are valid elevations or no data. In fact, since 0 or
below sea level
elevations are valid, \*(lqno data\*(rq can not be represented.
If the elevation layer contains no data values, either at its edges
or in its interior, incorrect (and usually quite large) slopes will
result.
The program can work in lat-lon coordinates and deals with global
wraparound.

.SH SEE ALSO
slope.aspect[1]
.SH "AUTHOR"
Michael Shapiro, U.S. Army Construction Engineering Research Laboratory
.br
Marjorie Larson, U.S. Army Construction Engineering Research Laboratory

