#include <math.h>
#include "gis.h"
#include "methods.h"


double bilinear (fd, window, cats, north, east, usedesc)
  int usedesc;
  double north, east;
  int fd;
  struct Cell_head window;
  struct Categories cats;
{
  /*
   * make the variables static to avoid them being redeclared
   * at each invocation and allow their values to be retained
   * between invocations.
   */
  static char		*buf;
  static CELL		*ptr;
  static int		row			= -1;
  static int		col			= -1;
  static int		old_row		= -1;
  static int		old_col		= -1;
  static int		load_a		=  0;
  static int		load_b		=  0;
  static int		window_rows	=  0;
  static int		window_cols	=  0;
  static double		grid[2][2], tmp1, tmp2;
  static double		predicted;


  if (!rload)
  {
    arow			= (arow) ? arow : G_allocate_cell_buf ();
    brow			= (brow) ? brow : G_allocate_cell_buf ();
  }


  window_rows		= (window_rows) ? window_rows : G_window_rows ();
  window_cols		= (window_cols) ? window_cols : G_window_cols ();


  /* convert northing and easting to row and col, resp */
  row = (int) G_northing_to_row (north, &window);
  col = (int) G_easting_to_col (east, &window);


  /*
   * we need 2x2 pixels to do the interpolation. First we decide if we
   * need the previous or next map row
   */
#if 0
  if (row == 0)
  {
    /* arow is at top, must get row below */
    if (G_get_map_row (fd, brow, row + 1) < 0)
      G_fatal_error ("Problem reading cell file");
  }
  else if (row+1 == G_window_rows ())
  {
    /* amaprow is at bottom, get the one above it */
    /* brow = arow; */
    for(i=0;i<G_window_cols();++i)
      brow[i]=arow[i];
    row--;
    if (G_get_map_row (fd, arow, row) < 0)
      G_fatal_error ("Problem reading cell file");
  }
  else if (north - G_row_to_northing ((double) row + 0.5, &window) > 0)
  {
    /* north is above a horizontal centerline going through arow */
    /* brow = arow; */
    for(i=0;i<G_window_cols();++i)
      brow[i]=arow[i];
    row--;
    if (G_get_map_row (fd, arow, row) < 0)
      G_fatal_error ("Problem reading cell file");
  }
  else
  {
    /* north is below a horizontal centerline going through arow */
    if (G_get_map_row (fd, brow, row+1) < 0)
      G_fatal_error ("Problem reading cell file");
  }
#endif


  if (row != 0)
    if ((row == window_rows - 1)
    /* arow is at bottom, get the one above it */
        || (north - G_row_to_northing ((double) row + 0.5, &window) > 0))
    /* north is above a horizontal centerline going through arow */
      row--;  /* adjust the top row value accordingly. */


  /*
   * Next, we decide if we need the column to the right or left of the
   * current column using a procedure similar to above
   */
  if ((col + 1 == window_cols)
      || (east - G_col_to_easting ((double) col + 0.5, &window) < 0))
    col--;


  /*
   * if the new row isn't the same as the old then load
   * the correct rows.
   */
  if (row != old_row)
  {
	load_a	= load_b	= 1;

	if (old_row != -1)
      switch (row - old_row)
      {
        case  1:  /* one row above previous */
          SWAP(arow , brow , ptr);
          load_b	= 0;
          break;
        case -1:  /* one row below previous */
          SWAP(arow , brow , ptr);
          load_a	= 0;
          break;
        default:
          break;
      }

    if (!rload)
    {
      if (load_a)
        if (G_get_map_row (fd , arow , row) < 0)
          G_fatal_error ("Problem reading cell file");
      if (load_b)
        if (G_get_map_row (fd , brow , row + 1) < 0)
          G_fatal_error ("Problem reading cell file");
    }
    else
    {
      arow		= (load_a) ? mem_rast[row    ] : arow;
      brow		= (load_b) ? mem_rast[row + 1] : brow;
    }
  }
  

  /*
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


  /*
   * Now we interpolate along a line parallel to columns
   * and passing through easting
   */


  old_row		= row;
  old_col		= col;

  predicted		= (north * tmp2 + (window.ns_res - north) * tmp1)
					/ window.ns_res;

  return predicted;
}
