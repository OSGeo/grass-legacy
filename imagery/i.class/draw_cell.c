#include <stdlib.h>
#include "globals.h"
#include "display.h"
#include "raster.h"
#include "local_proto.h"

int draw_cell (View *view, int overlay)
{
  int fd;
  int left, top;
  int ncols, nrows;
  int row;
  DCELL *dcell;
  struct Colors colr;
  int repeat;
  char msg[100];


  if (!view->cell.configured) return 0;
  if(G_read_colors (view->cell.name, view->cell.mapset, &colr) < 0)
    return 0;

  if (overlay == OVER_WRITE)
    display_title (view);

  D_set_colors (&colr);

  G_set_window (&view->cell.head);
  nrows = G_window_rows();
  ncols = G_window_cols();

  left = view->cell.left;
  top = view->cell.top;

  R_standard_color (BLUE);
  Outline_box (top, top+nrows-1, left, left+ncols-1);

  if (getenv("NO_DRAW"))
    {
      G_free_colors(&colr);
      return 1;
    }

  fd = G_open_cell_old (view->cell.name, view->cell.mapset);
  if (fd < 0)
    {
      G_free_colors (&colr);
      return 0;
    }
  dcell = G_allocate_d_raster_buf();


  sprintf (msg, "Plotting %s ...", view->cell.name);
  Menu_msg(msg);

  D_set_overlay_mode(!overlay);
  for (row = 0; row < nrows; row += repeat)
    {
      R_move_abs (left, top+row);
      if(G_get_d_raster_row_nomask(fd, dcell, row) < 0)
        break;
      repeat = G_row_repeat_nomask (fd, row);
      D_d_raster (dcell, ncols, repeat, &colr);
    }
  G_close_cell (fd);
  free (dcell);
  G_free_colors (&colr);

  return row==nrows;
}
