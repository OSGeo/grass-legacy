#include "gis.h"
#include "methods.h"


double nearest (fd, window, cats, north, east, usedesc)
  int usedesc;
  double north, east;
  int fd;
  struct Cell_head window;
  struct Categories cats;
{
  /*
   * declare these as static to allow values to be stored
   * between function invocations.
   */
  static char		*buf;
  static int		row			= -1;
  static int		col			= -1;
  static int		old_row 	= -1;
  static int		old_col		= -1;
  static double		predicted;

  if (!rload)
    /* first invocation of nearest () */
    arow	= (arow) ? arow : G_allocate_cell_buf ();

  /* convert northing and easting to row and col, resp */
  row = (int) G_northing_to_row (north, &window);
  col = (int) G_easting_to_col (east, &window);

  if (row != old_row)
  {
    if (rload)
      arow		= mem_rast[row];
    else if (G_get_map_row (fd, arow, row) < 0)
      G_fatal_error ("Problem reading cell file");
  }

  if (row != old_row || col != old_col)
  {
    if (usedesc)
    {
      G_squeeze(buf = G_get_cat (arow[col], &cats));
      predicted = scancatlabel(buf);
    }
    else
      predicted = arow[col];

    old_row		= row;
    old_col		= col;
  }

  return predicted;
}
