/*
 * Copyright (C) 1994. James Darrell McCauley.  (darrell@mccauley-usa.com)
 * 	                                        http://mccauley-usa.com/
 *
 * This program is free software under the GPL (>=v2)
 * Read the file GPL.TXT coming with GRASS for details.
 *
 * Reviewed by Jachym Cepicky 2005 (jachym.cepicky _at_ centrum _dot_ cz
 *                                  http://les-ejk.cz)
 */

#include "gis.h"
#include "nearest.h"

double nearest ( int fd, struct Cell_head *window, struct Categories *cats, 
	         double north, double east, int usedesc)
{
  int row, col;
  double predicted;
  DCELL *maprow = NULL;

  maprow = G_allocate_d_raster_buf ();

  /* convert northing and easting to row and col, resp */
  row = (int) G_northing_to_row (north, window);
  col = (int) G_easting_to_col (east, window);

  if (G_get_d_raster_row (fd, maprow, row) < 0)
  {
    fprintf(stderr,"row = (int) G_northing_to_row (north, &window);\n");
    fprintf(stderr,"G_get_map_row (fd, maprow, row) < 0\n");
    fprintf(stderr,"DIAG: \tRegion is: n=%g s=%g e=%g w=%g\n",
           window->north,window->south,window->east,window->west);
    fprintf(stderr,"      \tData point is north=%g east=%g\n",north,east);
    G_fatal_error ("\tProblem reading raster (e.g., use v.overlay to cut vector map)");
  }
  if (G_is_d_null_value(&(maprow[col]))) {
    predicted = 0.0;
  }
  else {
    predicted = maprow[col];
  }
  G_free(maprow);
  return predicted;
}
