


s.geom <contrib>      GRASS Reference Manual	 <contrib> s.geom



NAME
     s.geom - Computes Delaunay triangulation, MinMax-Angle
     triangulation, MinMax-Slope triangulation, MaxMin-Height
     triangulation, Regular triangulation, planesweep
     triangulation, Voronoi diagram, and convex hull of sites in
     2 and 2 1/2 dimensions.

SYNOPSIS
     s.geom
     s.geom  input = name  output = name [precision = val]
     [operation = val]

DESCRIPTION
     s.geom takes a sites file as input and computes various
     triangulations, the Voronoi diagram, or the convex hull of
     the sites. The z-coordinate is read from the description
     field if it is specified, otherwise 0 is assumed. The z-
     coordinate is used for the MinMax-slope triangulation and
     for the regular triangulation (where it is interpreted as
     the weight of the site). For all other computations the z-
     coordinate is ignored.

     In general there are many different triangulations for the
     same set of points. Several, of these triangulations are
     optimized for certain applications. The "quality" of most
     triangulations provided by s.geom is a function of the shape
     of the triangles in the triangulation.  For, example the
     Delaunay triangulation is the triangulation which maximizes
     the minimum angle of the triangles. This property makes the
     Delaunay triangulation useful for the finite elements method
     and surface interpolation, since the computational errors
     are reduced. A triangulation which is also frequently used
     for the same applications is the MinMax-angle triangulation,
     i.e. the triangulation that minimizes the maximum angle of
     the triangles. An other 2d triangulation provided by s.geom
     is the MaxMin-height triangulation. All of the
     triangulations mentioned above are triangulations of sites
     in the plane (i.e. the result is independent of the z-
     coordinate of the sites). The only "true" 2 1/2 dimensional
     triangulation provided by s.geom is the MinMax-slope
     triangulation, the triangulation which minimizes the maximum
     slope of the triangles. It should be mentioned, that all of
     the above triangulations not only optimize the triangle with
     the worst measure, but actually minimize (or maximize)
     (lexicographically) the sorted vector of the measure of all
     the triangles in the triangulation.

     The algorithms used for the computations are not heuristics,
     they actually achieve the optimum.

     The regular triangulation is the weighted version of the
     Delaunay triangulation (weights are assigned to the sites,



GRASS 4.2.1		Baylor University			1






s.geom <contrib>      GRASS Reference Manual	 <contrib> s.geom



     the Delaunay triangulation corresponds to the regular
     triangulation where all the sites have identical weights).

     The output is saved in vector file format.


PARAMETERS
     input	  Input sites file.

     output	  Output vector file.

     precision	  Number of significant positions after the
		  decimal point. (default is 0).

     operation	  One of the following: sweep, delaunay, angle,
		  height, slope, regular, voronoi, hull. These
		  correspond to the planesweep triangulation,
		  Delaunay triangulation, MinMax-angle
		  triangulation, MaxMin-height triangulation,
		  MinMax-slope triangulation, regular
		  triangulation, Voronoi diagram, and convex
		  hull, respectively.  (default is Delaunay
		  triangulation).

NOTES
     In this revised version of s.geom the coordinates saved in
     the output file are truncated to the chosen precision. (This
     does of course not include the voronoi vertices)

     After the triangulation is computed, it might happen
     (especially for the planesweep triangulation when used with
     data with many collinear points) that flat triangles remain,
     especially along the boundary. s.geom automatically tries to
     remove these triangles. However, the procedure used
     guarantees that no such triangle remains only in the case of
     the Delaunay triangulation.  If after this cleanup there are
     still some flat triangles remaining, s.geom notifies the
     user.  If these triangles are causing problems in subsequent
     computations, you can clean them up by using the output of
     s.geom as input for v.geom with operation = delaunay (Note,
     you should use the same precision in s.geom and v.geom).
     This will leave the triangulation essentially the same,
     specifically, every non-flat triangle will remain
     unmodified.

     Only the sites which fall into the current region are used
     for the computations.

     The computation times for the various operations depends
     strongly on the algorithm used:
     The planesweep triangulation and convex hull computation
     require O (n log n) operations in the worst case [Ed]. The



GRASS 4.2.1		Baylor University			2






s.geom <contrib>      GRASS Reference Manual	 <contrib> s.geom



     regular triangulation requires O (n log n) operations in
     expected case [EdSh]. The Delaunay triangulation and Voronoi
     diagram need O (n^2) time in the worst case, however it
     performs much faster in practice [Ed, La]. The MinMax-angle
     and MaxMin-height triangulations need O (n^2 log n)
     operations [BeEd, EdTa], and the MinMax-slope triangulation
     needs O (n^3) operations [BeEd].

     Internally, the coordinates of the sites are stored in fix-
     point format.  Therefore, the number of decimal digits
     cannot exceed 64 bit (or apprx.  16 decimal digits).


BUGS
     Some fields of the header in the output file are not
     properly set.

     An attempt was made to fix the bug which caused some of the
     edges in the Voronoi diagram to have arbitrary directions.

     In a previous version the voronoi regions incident to the
     eastern boundary were not closed. This should be fixed now.


SEE ALSO
     v.geom

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





GRASS 4.2.1		Baylor University			3






s.geom <contrib>      GRASS Reference Manual	 <contrib> s.geom



     [La] Lawson.  Software for C^1 surface interpolation.  Math.
     Software 1977, 161-194.


NOTICE
     This program is part of the contrib section of the GRASS
     distribution.  As such, it is externally contributed code
     that has not been examined or tested by the Office of GRASS
     Integration.














































GRASS 4.2.1		Baylor University			4



