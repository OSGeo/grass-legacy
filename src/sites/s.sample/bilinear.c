/*
 * Copyright (C) 1994. James Darrell McCauley.  (darrell@mccauley-usa.com)
 * 	                                        http://mccauley-usa.com/
 *
 * This program is free software under the GPL (>=v2)
 * Read the file GPL.TXT coming with GRASS for details.
 */

#include <unistd.h>
#include <math.h>
#include "gis.h"
#include "methods.h"

double bilinear (
  int fd,
  struct Cell_head window,
  struct Categories cats,
  double north,double east,
  int usedesc)
{
  char *buf;
  int i, row, col;
  double grid[2][2], tmp1, tmp2;
  DCELL *arow = NULL, *brow = NULL;

  arow = G_allocate_d_raster_buf ();
  brow = G_allocate_d_raster_buf ();

  /* convert northing and easting to row and col, resp */
  row = (int) G_northing_to_row (north, &window);
  col = (int) G_easting_to_col (east, &window);

  if (G_get_d_raster_row (fd, arow, row) < 0)
    G_fatal_error ("Problem reading cell file");
  /*
   * we need 2x2 pixels to do the interpolation. First we decide if we
   * need the previous or next map row
   */
  if (row == 0)
  {
    /* arow is at top, must get row below */
    if (G_get_d_raster_row (fd, brow, row + 1) < 0)
      G_fatal_error ("Problem reading cell file");
  }
  else if (row+1 == G_window_rows ())
  {
    /* amaprow is at bottom, get the one above it */
    /* brow = arow; */
    for(i=0;i<G_window_cols();++i)
      brow[i]=arow[i];
    row--;
    if (G_get_d_raster_row (fd, arow, row) < 0)
      G_fatal_error ("Problem reading cell file");
  }
  else if (north - G_row_to_northing ((double) row + 0.5, &window) > 0)
  {
    /* north is above a horizontal centerline going through arow */
    /* brow = arow; */
    for(i=0;i<G_window_cols();++i)
      brow[i]=arow[i];
    row--;
    if (G_get_d_raster_row (fd, arow, row) < 0)
      G_fatal_error ("Problem reading cell file");
  }
  else
  {
    /* north is below a horizontal centerline going through arow */
    if (G_get_d_raster_row (fd, brow, row+1) < 0)
      G_fatal_error ("Problem reading cell file");
  }

  /*
   * Next, we decide if we need the column to the right or left of the
   * current column using a procedure similar to above
   */
  if (col+1 == G_window_cols ())
    col--;
  else if (east - G_col_to_easting ((double) col + 0.5, &window) < 0)
    col--;

  /*-
   * now were ready to do bilinear interpolation over
   * arow[col], arow[col+1],
   * brow[col], brow[col+1]
   */
 
  if (usedesc)
  {
    G_squeeze(buf = G_get_cat (arow[col], &cats));
    grid[0][0] = scancatlabel (buf);
    G_squeeze(buf = G_get_cat (arow[col+1], &cats));
    grid[0][1] = scancatlabel (buf);
    G_squeeze(buf = G_get_cat (brow[col], &cats));
    grid[1][0] = scancatlabel (buf);
    G_squeeze(buf = G_get_cat (brow[col+1], &cats));
    grid[1][1] = scancatlabel (buf);
  }
  else
  {
    grid[0][0] = (double) arow[col];
    grid[0][1] = (double) arow[col + 1];
    grid[1][0] = (double) brow[col];
    grid[1][1] = (double) brow[col + 1];
  }

  /* Treat NULL's as zero */
  if (G_is_d_null_value(&(arow[col])))
    grid[0][0] = 0.0;
  if (G_is_d_null_value(&(arow[col+1])))
    grid[0][1] = 0.0;
  if (G_is_d_null_value(&(brow[col])))
    grid[1][0] = 0.0;
  if (G_is_d_null_value(&(brow[col+1])))
    grid[1][1] = 0.0;

  east=fabs(G_col_to_easting((double)col,&window)-east);
  while (east > window.ew_res)
    east-=window.ew_res;
  north=fabs(G_row_to_northing((double)row,&window)-north);
  while (north > window.ns_res)
    north-=window.ns_res;

  /* we do two linear interpolations along the rows */
  tmp1 = east * grid[0][1] + (window.ew_res - east) * grid[0][0];
  tmp1 /= window.ew_res;
  tmp2 = east * grid[1][1] + (window.ew_res - east) * grid[1][0];
  tmp2 /= window.ew_res;
/* fprintf(stderr,"DIAG: r=%d c=%d t1=%g t2=%g\te=%g n=%g\n",row,col,tmp1,tmp2,east,north); */
/* fprintf(stderr,"DIAG: %g %g\n      %g %g\n",grid[0][0],grid[0][1],grid[1][0],grid[1][1]); */

  /*-
   * Now we interpolate along a line parallel to columns
   * and passing through easting
   */
  G_free(arow);
  G_free(brow);
  return (north * tmp2 + (window.ns_res - north) * tmp1)
         /(window.ns_res);
}
