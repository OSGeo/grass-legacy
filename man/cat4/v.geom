


v.geom <contrib>      GRASS Reference Manual	 <contrib> v.geom



NAME
     v.geom - Computes constrained MinMax-Angle triangulation,
     constrained MinMax-Slope triangulation, constrained MaxMin-
     Height triangulation, constrained planesweep triangulation,
     constrained Delaunay triangulation, and convex hull of sites
     and prescribed edges in 2 and 2 1/2 dimensions.

SYNOPSIS
     v.geom
     v.geom  input = name  output = name [precision = val]
     [operation = val]

DESCRIPTION
     v.geom takes a vector file as input and computes various
     triangulations respecting the input edges, or the convex
     hull of the sites. The z-coordinate is read from the
     description field if it is specified, otherwise 0 is
     assumed. The z-coordinate is used for the MinMax-slope
     triangulation.  For all other computations the z-coordinate
     is ignored.

     In general there are many different triangulations for the
     same set of points. Several, of these triangulations are
     optimized for certain applications. The "quality" of most
     triangulations provided by v.geom is a function of the shape
     of the triangles in the triangulation.  For, example the
     Delaunay triangulation is the triangulation which maximizes
     the minimum angle of the triangles. This property makes the
     Delaunay triangulation useful for the finite elements method
     and surface interpolation, since the computational errors
     are reduced. A triangulation which is also frequently used
     for the same applications is the MinMax-angle triangulation,
     i.e. the triangulation that minimizes the maximum angle of
     the triangles. An other 2d triangulation provided by v.geom
     is the MaxMin-height triangulation. All of the
     triangulations mentioned above are triangulations of sites
     in the plane (i.e. the result is independent of the z-
     coordinate of the sites). The only "true" 2 1/2 dimensional
     triangulation provided by v.geom is the MinMax-slope
     triangulation, the triangulation which minimizes the maximum
     slope of the triangles. It should be mentioned, that all of
     the above triangulations not only optimize the triangle with
     the worst measure, but actually minimize (or maximize)
     (lexicographically) the sorted vector of the measure of all
     the triangles in the triangulation.

     The algorithms used for the computations are not heuristics,
     they actually achieve the optimum.

     All the algorithms used in v.geom are the same as in s.geom,
     except that prescribed edges (in the case of v.geom, the
     edges of the input vector map) are part of the



GRASS 4.2.1		Baylor University			1






v.geom <contrib>      GRASS Reference Manual	 <contrib> v.geom



     triangulation. This can be very useful, for example, when
     edges in the triangulation should be prevented from crossing
     certain features in the map.

     The output is saved in vector file format. Edge labels of
     input edges will also be attached to the corresponding
     output edges.


PARAMETERS
     input	  Input vector (level 2) file.

     output	  Output vector file.

     precision	  Number of significant positions after the
		  decimal point. (default is 0).

     operation	  One of the following: sweep, delaunay, angle,
		  height, slope, hull, readwrite. These
		  correspond to the constrained planesweep
		  triangulation, constrained Delaunay
		  triangulation, constrained MinMax-angle
		  triangulation, constrained MaxMin-height
		  triangulation, constrained MinMax-slope
		  triangulation, and convex hull, respectively.
		  (default is the constrained Delaunay
		  triangulation). The readwrite operation reads
		  the vector file, truncates the coordinates to
		  the defined precision and removes duplicate
		  points, zero length edges, and collinear
		  overlapping edges. An application of this
		  operation is described later in this man page.

NOTES
     In this revised version of v.geom the coordinates saved in
     the output file are truncated to the chosen precision.

     The computation times for the various operations depends
     strongly on the algorithm used:
     The planesweep triangulation and convex hull computation
     require O (n log n) operations in the worst case [Ed].  The
     Delaunay triangulation needs O (n^2) time in the worst case,
     however it performs much faster in practice. The MinMax-
     angle and MaxMin-height triangulations need O (n^2 log n)
     operations [BeEd, EdTa], and the MinMax-slope triangulation
     needs O (n^3) operations [BeEd].

     Internally, the coordinates of the sites are stored in fix-
     point format.  Therefore, the number of decimal digits
     cannot exceed 64 bit (or apprx.  16 decimal digits).

     It is important that the input vector file is reasonably



GRASS 4.2.1		Baylor University			2






v.geom <contrib>      GRASS Reference Manual	 <contrib> v.geom



     "clean". The current implementation of v.geom takes care of
     loops (i.e. zero length edges), duplicate edges, and edges
     which are collinear and overlapping.  However, because of
     the internal representation of coordinates in fix-point
     format it can happen that certain anomalies are introduced
     if the input-data is too dense.  For example edges can cross
     although they don't in the input data.  Currently, the
     program does not test for such cases. If it occurs one of
     two situations will happen. Either, the planesweep algorithm
     terminates with a segmentation fault, or it will loop
     forever.  If problems occur, apply the following 4 step
     solution.

     v.geom  in = your.data out = temp.data pr = PRECISION op =
	     readwrite

     v.spag  temp.data

     v.support	temp.data

     v.geom  in = temp.data out = out.data pr = PRECISION op =
	     TRIANGTYPE



     The first v.geom moves every vertex to it's new position (as
     specified by precision). As mentioned above this my cause
     lines to intersect.  Therefore, we apply v.spag and
     v.support to clean up the mess.  In the second application
     of v.geom only vertices created by v.spag will be moved.
     Hopefully, this will not cause any problems and the
     triangulation can now be computed.

     After the triangulation is computed, it might happen
     (especially for the planesweep triangulation when used with
     data with many collinear points) that flat triangles remain,
     especially along the boundary. v.geom automatically tries to
     remove these triangles. However, the procedure used
     guarantees that no such triangle remains only in the case of
     the Delaunay triangulation.  If after this cleanup there are
     still some flat triangles remaining, v.geom notifies the
     user.  If these triangles are causing problems in subsequent
     computations, you can clean them up by using the output of
     v.geom as input for a second round of v.geom with operation
     = delaunay (Note, you should use the same precision in both
     applications of v.geom).  This will leave the triangulation
     essentially the same, specifically, every non-flat triangle
     will remain unmodified.


BUGS
     Some fields of the header in the output file are not



GRASS 4.2.1		Baylor University			3






v.geom <contrib>      GRASS Reference Manual	 <contrib> v.geom



     properly set.

SEE ALSO
     s.geom,  v.spag, and v.support.

AUTHOR
     Roman Waupotitsch. waupo@cs.uiuc.edu

REFERENCES
     [BeEd] M.Bern, H. Edelsbrunner, D. Eppstein, S. Mitchel,
     T.S. Tan.	Edge Insertion for Optimal Triangulations.  In
     Proc. 1st Latin American Sympos. Theoret. Informatics 1992,
     46--60.


     [Ed] H. Edelsbrunner.  Algorithms in Combinatorial Geometry.
     Springer-Verlag, Heidelberg, Germany, 1987.


     [EdSh] H. Edelsbrunner, N. R. Shah.  Incremental Flipping
     Works for Regular Triangulations.	In Proc. 8th Ann. Sympos.
     Comput. Geom. 1992, 43-52.


     [EdTa] H. Edelsbrunner, T.S. Tan and R. Waupotitsch.  An
     O(n^2 log n) Time Algorithm for the MinMax Angle
     Triangulation.  SIAM J. Sci. Statist. Comput. 13 1992,
     994-1008.


     [La] Lawson.  Software for C^1 surface interpolation.  Math.
     Software 1977, 161-194.

NOTICE
     This program is part of the contrib section of the GRASS
     distribution.  As such, it is externally contributed code
     that has not been examined or tested by the Office of GRASS
     Integration.

















GRASS 4.2.1		Baylor University			4



