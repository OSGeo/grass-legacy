.TH v.geom
.SH NAME
\fIv.geom\fR \- Computes constrained MinMax-Angle triangulation, 
constrained MinMax-Slope triangulation, constrained 
MaxMin-Height triangulation, constrained planesweep triangulation, 
constrained Delaunay triangulation, 
and convex hull of sites and prescribed edges in 2 and 2 1/2 dimensions.
.br
.SH SYNOPSIS
\fBv.geom\fR
.br
\fBv.geom \fR \fBinput = name \fR \fBoutput = name\fR
[\fBprecision = val\fR] [\fBoperation = val\fR]
.br
.SH DESCRIPTION
\fIv.geom\fR takes a vector file as input and computes various triangulations
respecting the input edges, or
the convex hull of the sites. The z-coordinate
is read from the description field if it is specified, otherwise 0 is
assumed. The z-coordinate is used for the MinMax-slope triangulation.
For all other computations the z-coordinate is ignored.
.br

In general there are many different triangulations for the same set
of points. Several, of these triangulations are optimized for certain
applications. The "quality" of most triangulations provided by \fIv.geom\fR
is a function of the shape of the triangles in the triangulation.
For, example the Delaunay triangulation is the triangulation which
maximizes the minimum angle of the triangles. This property makes the
Delaunay triangulation useful for the finite elements method and surface
interpolation, since the computational errors are reduced. A triangulation 
which is also frequently used for the same applications is the MinMax-angle 
triangulation, i.e. the triangulation that minimizes the maximum angle of 
the triangles. An other 2d triangulation provided by \fIv.geom\fR is
the MaxMin-height triangulation. All of the triangulations mentioned above
are triangulations of sites in the plane (i.e. the result is independent of 
the z-coordinate of the sites). The only "true" 2 1/2 dimensional
triangulation provided by \fIv.geom\fR is the MinMax-slope
triangulation, the triangulation which minimizes the maximum slope of
the triangles. It should be mentioned, that all of the above
triangulations not only optimize the triangle with the worst measure,
but actually minimize (or maximize) (lexicographically) the sorted vector
of the measure of all the triangles in the triangulation.
.br

The algorithms used for the computations are not heuristics, they
actually achieve the optimum. 
.br

All the algorithms used in \fIv.geom\fR are the same as in
\fIs.geom\fR, except that prescribed edges (in the case of
\fIv.geom\fR, the edges of the input vector map) are part of the 
triangulation. This can be very useful, for example, when edges in the
triangulation should be prevented from crossing certain features in the
map.  
.br

The output is saved in vector file format. Edge labels of input edges 
will also be attached to the corresponding output edges.
.br

.SH PARAMETERS
.IP \fBinput\fR 13 
Input vector (level 2) file.
.IP \fBoutput\fR 13 
Output vector file.
.IP \fBprecision\fR 13 
Number of significant positions after the decimal point. (default is 0).
.IP \fBoperation\fR 13
One of the following: \fBsweep\fR, \fBdelaunay\fR, \fBangle\fR, \fBheight\fR,
\fBslope\fR, \fBhull\fR, \fBreadwrite\fR. These
correspond to the constrained planesweep 
triangulation, constrained Delaunay triangulation, constrained 
MinMax-angle triangulation, constrained MaxMin-height triangulation, 
constrained MinMax-slope triangulation, and convex hull, respectively.
(default is the constrained Delaunay triangulation). The
\fIreadwrite\fR operation reads the vector file, truncates the
coordinates to the defined precision and removes duplicate points,
zero length edges, and collinear overlapping edges. An application of
this operation is described later in this man page.
.br
.SH NOTES
In this revised version of \fIv.geom\fR the coordinates saved in the
output file are truncated to the chosen precision.
.br

The computation times for the various operations depends strongly on
the algorithm used:
.br
The planesweep triangulation and convex hull computation require
O (n log n) operations in the worst case [Ed]. 
The Delaunay triangulation needs O (n^2) time in the worst
case, however it performs much faster in practice. The MinMax-angle
and MaxMin-height triangulations need O (n^2 log n) operations 
[BeEd, EdTa], and the MinMax-slope triangulation needs O (n^3) operations
[BeEd].
.br

Internally, the coordinates of the sites are stored in fix-point format.
Therefore, the number of decimal digits cannot exceed 64 bit (or apprx.
16 decimal digits). 
.br

It is important that the input vector file is reasonably "clean". The
current implementation of \fBv.geom\fR takes care of loops (i.e. zero
length edges), duplicate edges, and edges which are collinear and overlapping.
However, because of the internal representation of coordinates in
fix-point format it can happen that certain anomalies are introduced if
the input-data is too dense.
For example edges can cross although they don't in the input data.
Currently, the program does not test for such cases. If it occurs
one of two situations will happen. Either, the planesweep algorithm
terminates with a segmentation fault, or it will loop forever. 
If problems occur, apply the following 4 step solution.
.IP \fIv.geom\fR 8
\fIin = \fRyour.data \fIout = \fRtemp.data \fIpr = \fRPRECISION \fIop = \fRreadwrite
.IP \fIv.spag\fR 8
temp.data
.IP \fIv.support\fR 11
temp.data
.IP \fIv.geom\fR 8
\fIin = \fRtemp.data \fIout = \fRout.data \fIpr = \fRPRECISION \fIop = \fRTRIANGTYPE

.IP \fI\fR 0
The first \fIv.geom\fR moves every vertex to it's new position (as specified
by \fIprecision\fR). As mentioned above this my cause lines to intersect.
Therefore, we apply \fIv.spag\fR and \fIv.support\fR to clean up the mess.
In the second application of \fIv.geom\fR only vertices created by
\fIv.spag\fR 
will be moved. Hopefully, this will not cause any problems and the 
triangulation can now be computed.
.br

After the triangulation is computed, it might happen (especially for
the planesweep triangulation when used with
data with many collinear points) that flat triangles remain,
especially along the boundary. \fIv.geom\fR
automatically tries to remove these triangles. However, the procedure used
guarantees that no such triangle remains only in the case of the
Delaunay triangulation. 
If after this cleanup there are still some flat triangles remaining,
\fIv.geom\fR notifies the user.
If these triangles are causing problems in subsequent 
computations, you can clean them up by using 
the output of \fIv.geom\fR as input for a second round of \fIv.geom\fR with 
\fIoperation = delaunay\fR (Note, you should use the same \fIprecision\fR 
in both applications of \fIv.geom\fR).
This will leave the triangulation essentially the
same, specifically, every non-flat triangle will remain unmodified.

.SH BUGS
Some fields of the header in the output file are not properly set.
.SH "SEE ALSO"
\fIs.geom\fR,  \fIv.spag\fR, and \fIv.support\fR.
.SH "AUTHOR"
Roman Waupotitsch. waupo@cs.uiuc.edu
.SH "REFERENCES"
.LP
[BeEd] M.Bern, H. Edelsbrunner, D. Eppstein, S. Mitchel, T.S. Tan.
Edge Insertion for Optimal Triangulations.
\fIIn Proc. 1st Latin American Sympos. Theoret. Informatics 1992\fR, 46--60.
.br

.LP
[Ed] H. Edelsbrunner.
\fIAlgorithms in Combinatorial Geometry.\fR
Springer-Verlag, Heidelberg, Germany, 1987.
.br

.LP
[EdSh] H. Edelsbrunner, N. R. Shah.
Incremental Flipping Works for Regular Triangulations.
\fIIn Proc. 8th Ann. Sympos. Comput. Geom. 1992\fR, 43-52.
.br

.LP
[EdTa] H. Edelsbrunner, T.S. Tan and R. Waupotitsch.
An O(n^2 log n) Time Algorithm for the MinMax Angle Triangulation.
\fISIAM J. Sci. Statist. Comput. 13 1992\fR, 994-1008.
.br

.LP
[La] Lawson.
Software for C^1 surface interpolation.
\fIMath. Software 1977\fR, 161-194.
