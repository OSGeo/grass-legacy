/*
 * Copyright (C) 1994. James Darrell McCauley.  (darrell@mccauley-usa.com)
 * 	                                        http://mccauley-usa.com/
 *
 * This program is free software under the GPL (>=v2)
 * Read the file GPL.TXT coming with GRASS for details.
 */

#include <stdlib.h>
#include <math.h>
#include "gis.h"
#include "methods.h"

/* #define DEBUG 1 */

double cubic (
  int fd,
  struct Cell_head window,
  struct Categories cats,
  double north,double east,
  int usedesc)
{
  char *buf;
  int i, row, col;
  double grid[4][4], tmp[4];
  DCELL *arow = NULL, *brow = NULL, *crow = NULL, *drow = NULL;

  arow = G_allocate_d_raster_buf ();
  brow = G_allocate_d_raster_buf ();
  crow = G_allocate_d_raster_buf ();
  drow = G_allocate_d_raster_buf ();

  /* convert northing and easting to row and col, resp */
  row = (int) G_northing_to_row (north, &window);
  col = (int) G_easting_to_col (east, &window);

  if (G_get_d_raster_row (fd, arow, row) < 0)
    G_fatal_error ("Problem reading cell file");

  /* we need 4x4 pixels to do the interpolation. */

  if (row == 0)
  {
    /* row containing sample is at top, must get three rows below */
    if (G_get_d_raster_row (fd, brow, row + 1) < 0)
      G_fatal_error ("Problem reading cell file");
    if (G_get_d_raster_row (fd, crow, row + 2) < 0)
      G_fatal_error ("Problem reading cell file");
    if (G_get_d_raster_row (fd, drow, row + 3) < 0)
      G_fatal_error ("Problem reading cell file");
  }
  else if (row == 1)
  {
    /* must get row above and tow rows below */
    for (i = 0; i < G_window_cols (); ++i)
      brow[i] = arow[i];
    if (G_get_d_raster_row (fd, arow, row - 1) < 0)
      G_fatal_error ("Problem reading cell file");
    if (G_get_d_raster_row (fd, crow, row + 1) < 0)
      G_fatal_error ("Problem reading cell file");
    if (G_get_d_raster_row (fd, drow, row + 2) < 0)
      G_fatal_error ("Problem reading cell file");
    row--;
  }
  else if (row + 1 == G_window_rows ())
  {
    /* arow is at bottom, get the three above it */
    for (i = 0; i < G_window_cols (); ++i)
      drow[i] = arow[i];
    if (G_get_d_raster_row (fd, arow, row - 3) < 0)
      G_fatal_error ("Problem reading cell file");
    if (G_get_d_raster_row (fd, brow, row - 2) < 0)
      G_fatal_error ("Problem reading cell file");
    if (G_get_d_raster_row (fd, crow, row - 1) < 0)
      G_fatal_error ("Problem reading cell file");
    row -= 3;
  }
  else if (row + 2 == G_window_rows ())
  {
    /* arow is next to bottom, get the one below and two above it */
    for (i = 0; i < G_window_cols (); ++i)
      crow[i] = arow[i];
    if (G_get_d_raster_row (fd, arow, row - 2) < 0)
      G_fatal_error ("Problem reading cell file");
    if (G_get_d_raster_row (fd, brow, row - 1) < 0)
      G_fatal_error ("Problem reading cell file");
    if (G_get_d_raster_row (fd, drow, row + 1) < 0)
      G_fatal_error ("Problem reading cell file");
    row -= 2;
  }
  else if (north - G_row_to_northing ((double) row + 0.5, &window) > 0)
  {
    /*
     * north is above a horizontal centerline going through arow. need two
     * above and one below
     */
    for (i = 0; i < G_window_cols (); ++i)
      crow[i] = arow[i];
    if (G_get_d_raster_row (fd, arow, row - 2) < 0)
      G_fatal_error ("Problem reading cell file");
    if (G_get_d_raster_row (fd, brow, row - 1) < 0)
      G_fatal_error ("Problem reading cell file");
    if (G_get_d_raster_row (fd, drow, row + 1) < 0)
      G_fatal_error ("Problem reading cell file");
    row -= 2;
  }
  else
  {
    /*
     * north is below a horizontal centerline going through arow need one
     * above and two below
     */
    for (i = 0; i < G_window_cols (); ++i)
      brow[i] = arow[i];
    if (G_get_d_raster_row (fd, arow, row - 1) < 0)
      G_fatal_error ("Problem reading cell file");
    if (G_get_d_raster_row (fd, crow, row + 1) < 0)
      G_fatal_error ("Problem reading cell file");
    if (G_get_d_raster_row (fd, drow, row + 2) < 0)
      G_fatal_error ("Problem reading cell file");
    row--;
  }

  /*
   * Next, we decide if we need columns to the right and/or left of the
   * current column using a procedure similar to above
   */
  if (col == 0 || col == 1)
    col = 0;
  else if (col + 1 == G_window_cols ())
    col -= 3;
  else if (col + 2 == G_window_cols ())
    col -= 2;
  else if (east - G_col_to_easting ((double) col + 0.5, &window) < 0)
    /* east is left of center */
    col -= 2;
  else
    col--;

  /*-
   * now were ready to do bilinear interpolation over
   * arow[col], arow[col+1], arow[col+2], arow[col+3],
   * brow[col], brow[col+1], brow[col+2], brow[col+3],
   * crow[col], crow[col+1], crow[col+2], crow[col+3],
   * drow[col], drow[col+1], drow[col+2], drow[col+3],
   */

  if (usedesc)
  {
    for (i = 0; i < 4; ++i)
    {
      G_squeeze (buf = G_get_cat (arow[col + i], &cats));
      grid[0][i] = scancatlabel (buf);
    }
    for (i = 0; i < 4; ++i)
    {
      G_squeeze (buf = G_get_cat (brow[col + i], &cats));
      grid[1][i] = scancatlabel (buf);
    }
    for (i = 0; i < 4; ++i)
    {
      G_squeeze (buf = G_get_cat (crow[col + i], &cats));
      grid[2][i] = scancatlabel (buf);
    }
    for (i = 0; i < 4; ++i)
    {
      G_squeeze (buf = G_get_cat (drow[col + i], &cats));
      grid[3][i] = scancatlabel (buf);
    }
  }
  else
  {
    for (i = 0; i < 4; ++i)
      grid[0][i] = (double) arow[col + i];
    for (i = 0; i < 4; ++i)
      grid[1][i] = (double) brow[col + i];
    for (i = 0; i < 4; ++i)
      grid[2][i] = (double) crow[col + i];
    for (i = 0; i < 4; ++i)
      grid[3][i] = (double) drow[col + i];
  }

  /* Treat NULL cells as 0.0 */
  for (i = 0; i < 4 ; i++) {
    if (G_is_d_null_value(&(arow[col + i]))) {
	    grid[0][i] = 0.0;
    }
  }
  for (i = 0; i < 4 ; i++) {
    if (G_is_d_null_value(&(brow[col + i]))) {
	    grid[1][i] = 0.0;
    }
  }
  for (i = 0; i < 4 ; i++) {
    if (G_is_d_null_value(&(crow[col + i]))) {
	    grid[2][i] = 0.0;
    }
  }
  for (i = 0; i < 4 ; i++) {
    if (G_is_d_null_value(&(drow[col + i]))) {
	    grid[3][i] = 0.0;
    }
  }
    
  /* this needs work here */
  east = fabs (G_col_to_easting ((double) col + 1, &window) - east);
  while (east > window.ew_res)
    east -= window.ew_res;
  east /= window.ew_res;
  north = fabs (G_row_to_northing ((double) row + 1, &window) - north);
  while (north > window.ns_res)
    north -= window.ns_res;
  north /= window.ns_res;

  /* we do four cubic convolutions along the rows */
  for (i = 0; i < 4; ++i)
    tmp[i] = east * (east * (east * (grid[i][3] - grid[i][2]
				     + grid[i][1] - grid[i][0])
			     + (grid[i][2] - grid[i][3] - 2 * grid[i][1]
				+ 2 * grid[i][0]))
		     + (grid[i][2] - grid[i][0])) + grid[i][1];

#ifdef DEBUG
  for (i = 0; i < 4; ++i) {
    for (j = 0; j < 4; ++j)
      fprintf (stderr, "%g ", grid[i][j]);
    fprintf (stderr, "\n");
  }
  fprintf (stderr, "DIAG: (%d,%d) 1=%3.2g 2=%3.2g 3=%3.2g 4=%3.2g\te=%g n=%g\n",
	   row, col, tmp[0], tmp[1], tmp[2], tmp[3], east, north);
#endif

  free(arow);
  free(brow);
  free(crow);
  free(drow);

  /* user horner's method again for the final interpolation */
  return (north * (north * (north * (tmp[3] - tmp[2]
				     + tmp[1] - tmp[0])
			    + (tmp[2] - tmp[3] - 2 * tmp[1]
			       + 2 * tmp[0]))
		   + (tmp[2] - tmp[0])) + tmp[1]);
}

