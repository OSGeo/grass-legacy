/*
 * Copyright (C) 1994. James Darrell McCauley.  (darrell@mccauley-usa.com)
 * 	                                        http://www.usol.com/~mccauley/
 *
 * This program is free software under the GPL (>=v2)
 * Read the file GPL.TXT coming with GRASS for details.
 */

#include "gis.h"
#include "methods.h"

double nearest (fd, window, cats, north, east, usedesc)
  int usedesc;
  double north, east;
  int fd;
  struct Cell_head window;
  struct Categories cats;
{
  char *buf;
  int row, col;
  double predicted;
  CELL *maprow = NULL;

  maprow = G_allocate_cell_buf ();

  /* convert northing and easting to row and col, resp */
  row = (int) G_northing_to_row (north, &window);
  col = (int) G_easting_to_col (east, &window);

  if (G_get_map_row (fd, maprow, row) < 0)
  {
    fprintf(stderr,"row = (int) G_northing_to_row (north, &window);\n");
    fprintf(stderr,"G_get_map_row (fd, maprow, row) < 0\n");
    fprintf(stderr,"DIAG: \tRegion is: n=%g s=%g e=%g w=%g\n",
           window.north,window.south,window.east,window.west);
    fprintf(stderr,"      \tData point is north=%g east=%g\n",north,east);
    G_fatal_error ("\tProblem reading cell file");
  }
  if (usedesc)
  {
    G_squeeze(buf = G_get_cat (maprow[col], &cats));
    predicted=scancatlabel(buf);
  }
  else
    predicted = maprow[col];

  return predicted;
}
