.TH s.geom
.SH NAME
\fIs.geom\fR \- Computes Delaunay triangulation, MinMax-Angle triangulation, 
MinMax-Slope triangulation, MaxMin-Height triangulation,
Regular triangulation, planesweep triangulation, 
Voronoi diagram, and convex hull of sites in 2 and 
2 1/2 dimensions.
.br
.SH SYNOPSIS
\fBs.geom\fR
.br
\fBs.geom \fR \fBinput = name \fR \fBoutput = name\fR
[\fBprecision = val\fR] [\fBoperation = val\fR]
.br
.SH DESCRIPTION
\fIs.geom\fR takes a sites file as input and computes various triangulations,
the Voronoi diagram, or the convex hull of the sites. The z-coordinate
is read from the description field if it is specified, otherwise 0 is
assumed. The z-coordinate is used for the MinMax-slope triangulation
and for the regular triangulation (where it is interpreted as the
weight of the site). For all other computations the z-coordinate is ignored.
.br

In general there are many different triangulations for the same set
of points. Several, of these triangulations are optimized for certain
applications. The "quality" of most triangulations provided by \fIs.geom\fR
is a function of the shape of the triangles in the triangulation.
For, example the Delaunay triangulation is the triangulation which
maximizes the minimum angle of the triangles. This property makes the
Delaunay triangulation useful for the finite elements method and surface
interpolation, since the computational errors are reduced. A triangulation 
which is also frequently used for the same applications is the MinMax-angle 
triangulation, i.e. the triangulation that minimizes the maximum angle of 
the triangles. An other 2d triangulation provided by \fIs.geom\fR is
the MaxMin-height triangulation. All of the triangulations mentioned above
are triangulations of sites in the plane (i.e. the result is independent of 
the z-coordinate of the sites). The only "true" 2 1/2 dimensional
triangulation provided by \fIs.geom\fR is the MinMax-slope
triangulation, the triangulation which minimizes the maximum slope of
the triangles. It should be mentioned, that all of the above
triangulations not only optimize the triangle with the worst measure,
but actually minimize (or maximize) (lexicographically) the sorted vector
of the measure of all the triangles in the triangulation.
.br

The algorithms used for the computations are not heuristics, they
actually achieve the optimum. 
.br

The regular triangulation is the weighted version of the Delaunay
triangulation (weights are assigned to the sites, the Delaunay
triangulation corresponds to the regular triangulation where all the
sites have identical weights).
.br

The output is saved in vector file format. 
.br

.SH PARAMETERS
.IP \fBinput\fR 13 
Input sites file.
.IP \fBoutput\fR 13 
Output vector file.
.IP \fBprecision\fR 13 
Number of significant positions after the decimal point. (default is 0).
.IP \fBoperation\fR 13
One of the following: \fBsweep\fR, \fBdelaunay\fR, \fBangle\fR, \fBheight\fR,
\fBslope\fR, \fBregular\fR, \fBvoronoi\fR, \fBhull\fR. These
correspond to the planesweep 
triangulation, Delaunay triangulation, MinMax-angle triangulation,
MaxMin-height triangulation, MinMax-slope triangulation, regular
triangulation, Voronoi diagram, and convex hull, respectively.
(default is Delaunay triangulation).
.br
.SH NOTES
In this revised version of \fIs.geom\fR the coordinates saved in the
output file are truncated to the chosen precision. (This does of course
not include the voronoi vertices)
.br 

After the triangulation is computed, it might happen (especially for
the planesweep triangulation when used with
data with many collinear points) that flat triangles remain,
especially along the boundary. \fIs.geom\fR
automatically tries to remove these triangles. However, the procedure used
guarantees that no such triangle remains only in the case of the
Delaunay triangulation. 
If after this cleanup there are still some flat triangles remaining,
\fIs.geom\fR notifies the user.
If these triangles are causing problems in subsequent 
computations, you can clean them up by using 
the output of \fIs.geom\fR as input for \fIv.geom\fR with 
\fIoperation = delaunay\fR (Note, you should use the same \fIprecision\fR 
in \fIs.geom\fR and \fIv.geom\fR).
This will leave the triangulation essentially the
same, specifically, every non-flat triangle will remain unmodified.
.br 

Only the sites which fall into the current region are used for the 
computations.
.br

The computation times for the various operations depends strongly on
the algorithm used:
.br
The planesweep triangulation and convex hull computation require
O (n log n) operations in the worst case [Ed]. The regular triangulation
requires O (n log n) operations in expected case [EdSh]. The Delaunay
triangulation and Voronoi diagram need O (n^2) time in the worst
case, however it performs much faster in practice [Ed, La]. The MinMax-angle
and MaxMin-height triangulations need O (n^2 log n) operations 
[BeEd, EdTa], and the MinMax-slope triangulation needs O (n^3) operations
[BeEd].
.br

Internally, the coordinates of the sites are stored in fix-point format.
Therefore, the number of decimal digits cannot exceed 64 bit (or apprx.
16 decimal digits). 

.SH BUGS
Some fields of the header in the output file are not properly set.
.br

An attempt was made to fix the bug which caused some of the edges in the
Voronoi diagram to have arbitrary directions. 
.br

In a previous version the voronoi regions incident to the eastern boundary
were not closed. This should be fixed now.

.SH "SEE ALSO"
.I v.geom
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

