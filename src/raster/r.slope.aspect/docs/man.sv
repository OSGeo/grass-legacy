.TH Gslope.aspect 2 G-language
.SH NAME
Gslope.aspect \- generate slope and aspect maps from elevation
.br
(G language tool)
.I (GRASS-GRID Tool)
.SH SYNOPSIS
.nf
Gslope.aspect [-v] elevation=input slope=output1 aspect=output2
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

Often slope is represented in percent rise.  The following conversion
table is presented which converts from degrees to percent:

.NF
.ne 19
deg  perc   deg  perc   deg  perc   deg  perc   deg  perc   deg  perc

 0     0  | 15    27  | 30    58  | 45   100   | 60  173   | 75   373
 1     2  | 16    29  | 31    60  | 46   104   | 61  180   | 76   401
 2     3  | 17    31  | 32    62  | 47   107   | 62  188   | 77   433
 3     5  | 18    32  | 33    65  | 48   111   | 63  196   | 78   470
 4     7  | 19    34  | 34    67  | 49   115   | 64  205   | 79   514
 5     9  | 20    36  | 35    70  | 50   119   | 65  214   | 80   567
 6    11  | 21    38  | 36    73  | 51   123   | 66  225   | 81   631
 7    12  | 22    40  | 37    75  | 52   128   | 67  236   | 82   712
 8    14  | 23    42  | 38    78  | 53   133   | 68  248   | 83   814
 9    16  | 24    45  | 39    81  | 54   138   | 69  261   | 84   951
10    18  | 25    47  | 40    84  | 55   143   | 70  275   | 85  1143
11    19  | 26    49  | 41    87  | 56   148   | 71  290   | 86  1430
12    21  | 27    51  | 42    90  | 57   154   | 72  308   | 87  1908
13    23  | 28    53  | 43    93  | 58   160   | 73  327   | 88  2864
14    25  | 29    55  | 44    97  | 59   166   | 74  349   | 89  5729

               90 (undefined)
.FI

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

.SH SEE ALSO
slope.aspect[1]
.SH "AUTHOR"
Michael Shapiro, U.S. Army Construction Engineering Research Laboratory
.br
Marjorie Larson, U.S. Army Construction Engineering Research Laboratory

This version of r.slope.aspect works for lat-lon
(except for regions that have common east and west edges).

Additional features include:
   ability to specify if the elevation units are not in meters
   to choose the format of slope answer: degrees or % slope
   aspect is now 360 degrees instead of 24 categories of aspect

