


s.vol.rst <alpha>     GRASS Reference Manual	<alpha> s.vol.rst



NAME
     s.vol.rst (GRASS 3D Program)


SYNOPSIS
     s.vol.rst input=name] z_orig= value
      [tension=value] [maskmap= name ] [npmin=value] [zmult=
     value] [gradient=name] [aspect2= name] [mcurv=name]


DESCRIPTION
     s.vol.rst interpolates the values to 3-dimensional grid from
     point data (climatic stations, drill holes etc.)  given in a
     sites file named input.  The 3-dimensional grid is given by
     the current region as well as options z_orig, tb_res, n_levs
     which specify the third dimension of the grid.  If the
     options cellinp and cellout are specified then the output
     raster file cellout contains crossection of interpolated
     volume with surface defined by input cell file cellinp.  As
     an option, simultaneously with interpolation, topographic
     parameters gradient, both aspects, change of gradient,
     Gaussian curvature, or mean curvature are computed and saved
     as grid3 file as specified by the options gradient, aspect1,
     aspect2, ncurv, gcurv, mcurv respectively.


     At first, data points are checked for identical points and
     points that are closer to each other than given dmin are
     removed.  Parameters wmult allow user to re-scale the w-
     values and z-values for sites (useful e.g. for
     transformation of elevations given in feet to meters, so
     that the proper values of gradient and curvatures can be
     computed).



     Regularized spline with tension is used for the
     interpolation.  The tension parameter tunes the character of
     the resulting volume from thin plate to membrane.	Higher
     values of tension parameter reduce the overshoots that can
     appear in volumes with rapid change of gradient.  For noisy
     data, it is possible to define a smoothing parameter,
     smooth.  With the smoothing parameter set to zero (smooth=0)
     the resulting volume passes exactly through the data points.



     User can define a raster file named maskmap, which will be
     used as a mask. The interpolation is skipped for
     3-dimensional cells whose 2-dimensional projection has zero
     value in mask. Zero values will be assigned to these cells
     in all output grid3 files.



GRASS 5.0beta7	      GRASS Development Team			1






s.vol.rst <alpha>     GRASS Reference Manual	<alpha> s.vol.rst



     If the number of given points is greater than 400, segmented
     processing is used.  The region is split into 3-dimensional
     "box" segments, each having less than segmax points and
     interpolation is performed on each segment of the region. To
     ensure the smooth connection of segments the interpolation
     function for each segment is computed using the points in
     given segment and the points in its neighborhood. The
     minimum number of points taken for interpolation is
     controlled by npmin , the value of which must be larger than
     segmax and less than 400.	This limit of 400 was selected to
     ensure the numerical stability and efficiency of the
     algorithm.



     s.vol.rst uses regularized spline with tension for
     interpolation from point data (as described in Mitasova and
     Mitas, submitted to Mathematical Geology.). The
     implementation has an improved segmentation procedure based
     on Oct-trees which enhances the efficiency for large data
     sets.



     Topographic parameters are computed directly from the
     interpolation function so that the important relationships
     between these parameters are preserved.  Original values of
     curvatures are multiplied by 100000, to conform with GRASS
     integer raster files. Therefore any curvature lower than
     0.00001 will be zero.



     The program gives warning when significant overshoots appear
     and higher tension should be used.	 However, with tension
     too high the resulting volume changes its behavior to
     membrane( rubber sheet stretched over the data points
     resulting in a peak in each given point and everywhere else
     the volume goes rapidly to trend).	 With smoothing parameter
     greater than zero the volume will not pass through the data
     points and the higher the parameter the closer the volume
     will be to the trend. For theory on smoothing with splines
     see Talmi and Gilat, 1977 or Wahba, 1990.



     If a visible connection of segments appears, the program
     should be rerun with higher npmin to get more points from
     the neighborhood of given segment.






2		      GRASS Development Team	   GRASS 5.0beta7






s.vol.rst <alpha>     GRASS Reference Manual	<alpha> s.vol.rst



     If the number of points in a site file is less then 400,
     segmax should be set to 400 so that segmentation is not
     performed when it is not necessary.



     The program gives warning when user wants to interpolate
     outside the "box" given by minimum and maximum coordinates
     in site file, zoom into the area where the points are is
     suggested in this case.



     For large data sets (thousands of data points) it is
     suggested to zoom into a smaller representative area and
     test whether the parameters chosen (e.g. defaults) are
     appropriate.



     The user must run g.region before the program to set the
     region for interpolation.



Parameters:
     Name of the site file with input x,y,z,w

     Name of the surface cell file

     Minimum z-value

     Top-Bottom Resolution (delta z)

     Number of levels

     Tension Default: 40

     Smoothing parameter Default: 0

     Name of the raster file used as mask

     Max number of points in segment (=400) Default: 50

     Min distance between points (extra points ignored) Default:
     2.500000

     Min number of points for interpolation Default: 100

     Conversion factor for w-values Default: 1.0

     Conversion factor for z-values Default: 1.0



GRASS 5.0beta7	      GRASS Development Team			3






s.vol.rst <alpha>     GRASS Reference Manual	<alpha> s.vol.rst



     Name of the crossection cell file

     Elevation g3d-file

     Gradient g3d-file

     Aspect1 g3d-file

     Aspect2 g3d-file

     Change of gradient g3d-file

     Gaussian curvature g3d-file

     Mean curvature g3d-file



SEE ALSO
     g.region


AUTHOR
     Jaro Hofierka hofierka@geomodel.sk The development of
     s.vol.rst was funded by GeoModel s.r.o. company
     (www.geomodel.sk).


NOTICE
     This program is part of the alpha section of the GRASS
     distribution.  Unlike the code in the main section of GRASS,
     the alpha code has not yet been fully tested for one release
     cycle.






















4		      GRASS Development Team	   GRASS 5.0beta7



