#include <math.h>
#include "gis.h"
#include "methods.h"

/* #define DEBUG 1 */


double cubic (fd, window, cats, north, east, usedesc)
  int usedesc;
  double north, east;
  int fd;
  struct Cell_head window;
  struct Categories cats;
{
  /*
   * static variables allow for intermediate results to be
   * stored between invocations because the variables are
   * not redeclared for each invocation of the function.
   */
  static char		*buf;
  static CELL		*ptr;
  static int		i;
  static int		row			= -1;
  static int		col			= -1;
  static int		old_row		= -1;
  static int		old_col		= -1;
  static int		window_rows	=  0;
  static int		window_cols	=  0;
  static int		load_a		=  0;
  static int		load_b		=  0;
  static int		load_c		=  0;
  static int		load_d		=  0;
  static double		grid[4][4], tmp[4];
  static double		predicted;


  if (!rload)
  {
    arow	= (arow) ? arow : G_allocate_cell_buf ();
    brow	= (brow) ? brow : G_allocate_cell_buf ();
    crow	= (crow) ? crow : G_allocate_cell_buf ();
    drow	= (drow) ? drow : G_allocate_cell_buf ();
  }


  /* convert northing and easting to row and col, resp */
  row = (int) G_northing_to_row (north, &window);
  col = (int) G_easting_to_col (east, &window);


  if (row != 0) {
    if (row == 1)
      /* must get row above and two rows below */
      row--;
    else if (row == window_rows - 1)
      /* arow is at bottom, get the three above it */
      row	-= 3;
    else if (row == window_rows - 2)
      /* arow is next to bottom, get the one below and two above it */
      row	-= 2;
    else if (north - G_row_to_northing ((double) row + 0.5, &window) > 0)
      /*
       * north is above a horizontal centerline going through arow. need two
       * above and one below
       */
      row	-= 2;
    else
      /*
       * north is below a horizontal centerline going through arow need one
       * above and two below
       */
      row--;
  }

#if 0
  if (G_get_map_row (fd, arow, row) < 0)
    G_fatal_error ("Problem reading cell file");

  /* we need 4x4 pixels to do the interpolation. */

  if (row == 0)
  {
    /* row containing sample is at top, must get three rows below */
    if (G_get_map_row (fd, brow, row + 1) < 0)
      G_fatal_error ("Problem reading cell file");
    if (G_get_map_row (fd, crow, row + 2) < 0)
      G_fatal_error ("Problem reading cell file");
    if (G_get_map_row (fd, drow, row + 3) < 0)
      G_fatal_error ("Problem reading cell file");
  }
  else if (row == 1)
  {
    /* must get row above and two rows below */
    for (i = 0; i < G_window_cols (); ++i)
      brow[i] = arow[i];
    if (G_get_map_row (fd, arow, row - 1) < 0)
      G_fatal_error ("Problem reading cell file");
    if (G_get_map_row (fd, crow, row + 1) < 0)
      G_fatal_error ("Problem reading cell file");
    if (G_get_map_row (fd, drow, row + 2) < 0)
      G_fatal_error ("Problem reading cell file");
    row--;
  }
  else if (row + 1 == G_window_rows ())
  {
    /* arow is at bottom, get the three above it */
    for (i = 0; i < G_window_cols (); ++i)
      drow[i] = arow[i];
    if (G_get_map_row (fd, arow, row - 3) < 0)
      G_fatal_error ("Problem reading cell file");
    if (G_get_map_row (fd, brow, row - 2) < 0)
      G_fatal_error ("Problem reading cell file");
    if (G_get_map_row (fd, crow, row - 1) < 0)
      G_fatal_error ("Problem reading cell file");
    row -= 3;
  }
  else if (row + 2 == G_window_rows ())
  {
    /* arow is next to bottom, get the one below and two above it */
    for (i = 0; i < G_window_cols (); ++i)
      crow[i] = arow[i];
    if (G_get_map_row (fd, arow, row - 2) < 0)
      G_fatal_error ("Problem reading cell file");
    if (G_get_map_row (fd, brow, row - 1) < 0)
      G_fatal_error ("Problem reading cell file");
    if (G_get_map_row (fd, drow, row + 1) < 0)
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
    if (G_get_map_row (fd, arow, row - 2) < 0)
      G_fatal_error ("Problem reading cell file");
    if (G_get_map_row (fd, brow, row - 1) < 0)
      G_fatal_error ("Problem reading cell file");
    if (G_get_map_row (fd, drow, row + 1) < 0)
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
    if (G_get_map_row (fd, arow, row - 1) < 0)
      G_fatal_error ("Problem reading cell file");
    if (G_get_map_row (fd, crow, row + 1) < 0)
      G_fatal_error ("Problem reading cell file");
    if (G_get_map_row (fd, drow, row + 2) < 0)
      G_fatal_error ("Problem reading cell file");
    row--;
  }
#endif

  /*
   * Next, we decide if we need columns to the right and/or left of the
   * current column using a procedure similar to above
   */
  if (col == 0 || col == 1)
    col = 0;
  else if (col + 1 == window_cols)
    col -= 3;
  else if (col + 2 == window_cols)
    col -= 2;
  else if (east - G_col_to_easting ((double) col + 0.5, &window) < 0)
    /* east is left of center */
    col -= 2;
  else
    col--;


  /*
   * determine where the top row is relative to the previously
   * loaded rows
   */
  if (row != old_row)
  {
    load_a		= load_b	= load_c	= load_d	= 1;
    if (old_row != -1)
      switch (row - old_row)
      {
        case  3:  /* 3 above previous */
          SWAP(arow , drow , ptr);
          load_d	= 0;
          break;
        case  2:  /* 2 above previous */
          SWAP(brow , drow , ptr);
          SWAP(arow , crow , ptr);
          load_c	= load_d	= 0;
          break;
        case  1:  /* 1 above previous */
          SWAP(crow , drow , ptr);
          SWAP(brow , crow , ptr);
          SWAP(arow , brow , ptr);
          load_b	= load_c	= load_d	= 0;
          break;
        case -1:  /* 1 below previous */
          SWAP(brow , arow , ptr);
          SWAP(crow , brow , ptr);
          SWAP(drow , crow , ptr);
          load_a	= load_b	= load_c	= 0;
          break;
        case -2:  /* 2 below previous */
          SWAP(crow , arow , ptr);
          SWAP(drow , brow , ptr);
          load_a	= load_b	= 0;
          break;
        case -3:  /* 3 below previous */
          SWAP(drow , arow , ptr);
          load_a	= 0;
          break;
        default:
          break;
    }

    if (!rload)
    {
      if (load_a)
        if (G_get_map_row (fd , arow , row    ) < 0)
          G_fatal_error ("Problem reading cell file");
      if (load_b)
        if (G_get_map_row (fd , brow , row + 1) < 0)
          G_fatal_error ("Problem reading cell file");
      if (load_c)
        if (G_get_map_row (fd , crow , row + 2) < 0)
          G_fatal_error ("Problem reading cell file");
      if (load_d)
        if (G_get_map_row (fd , drow , row + 3) < 0)
          G_fatal_error ("Problem reading cell file");
    }
    else
    {
      arow		= (load_a) ? mem_rast[row    ] : arow;
      brow		= (load_b) ? mem_rast[row + 1] : brow;
      crow		= (load_c) ? mem_rast[row + 2] : crow;
      drow		= (load_d) ? mem_rast[row + 3] : drow;
    }
  }

  /*
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


  old_row		= row;
  old_col		= col;


  /* user horner's method again for the final interpolation */
  predicted		= (north * (north * (north *
                        (tmp[3] - tmp[2] + tmp[1] - tmp[0])
			           + (tmp[2] - tmp[3] - 2 * tmp[1]
			         + 2 * tmp[0]))
		           + (tmp[2] - tmp[0])) + tmp[1]);

  return predicted;
}

