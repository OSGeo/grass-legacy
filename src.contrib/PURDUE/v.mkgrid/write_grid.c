#include <stdio.h>
#include "grid_structs.h"
#include "Vect.h"

write_grid (fp_digit, grid_info, Map, quiet)
  FILE *fp_digit;
  struct grid_description *grid_info;
  struct Map_info *Map;
  int quiet;
{

  int i, k;
  int rows, cols;
  int num_v_rows, num_v_cols;
  double x, y;
  double sx, sy;
  double width, length;
  double next_x, next_y;
  double snext_x, snext_y;
  double angle, dum;
  char buffer[128];

  /* new with Vlib */
  struct line_pnts *Points;

  Points = Vect_new_line_struct ();

  num_v_rows = grid_info->num_vect_rows;
  num_v_cols = grid_info->num_vect_cols;
  rows = grid_info->num_rows;
  cols = grid_info->num_cols;
  width = grid_info->width;
  length = grid_info->length;
  angle = grid_info->angle;

  /* write out all the vector lengths (x vectors) of the entire grid  */
  if (!quiet)
    printf ("\n Writing out vector rows...");

  y = grid_info->origin_y;
  for (i = 0; i < num_v_rows; ++i)
  {
    x = grid_info->origin_x;
    for (k = 0; k < cols; ++k)
    {
      next_x = x + length;

      sx = x;
      sy = y;
      snext_x = next_x;
      dum = y;

      rotate (&x, &y, grid_info->origin_x, grid_info->origin_y, angle);
      rotate (&next_x, &dum, grid_info->origin_x, grid_info->origin_y, angle);
      write_vect (fp_digit, x, y, next_x, dum, Map, Points);

      y = sy;
      x = next_x = snext_x;
    }
    y += width;
  }

  /* write out all the vector widths (y vectors) of the entire grid  */
  if (!quiet)
    printf ("\n Writing out vector columns...");
  x = grid_info->origin_x;
  for (k = 0; k < num_v_cols; ++k)
  {
    y = grid_info->origin_y;
    for (i = 0; i < rows; ++i)
    {
      next_y = y + width;

      sx = x;
      sy = y;
      snext_y = next_y;
      dum = x;
      rotate (&x, &y, grid_info->origin_x, grid_info->origin_y, angle);
      rotate (&dum, &next_y, grid_info->origin_x, grid_info->origin_y, angle);

      write_vect (fp_digit, x, y, dum, next_y, Map, Points);

      x = sx;
      y = next_y = snext_y;
    }
    x += length;
  }

  /* new with Vlib */
  Vect_destroy_line_struct (Points);

  return (0);
}

static double xarray[10];
static double yarray[10];

#define  NUM_POINTS  2

write_vect (fp_digit, x1, y1, x2, y2, Map, Points)
  FILE *fp_digit;
  double x1, y1, x2, y2;
  struct Map_info *Map;
  struct line_pnts *Points;	/* new with Vlib */
{

  xarray[0] = x1;
  xarray[1] = x2;
  yarray[0] = y1;
  yarray[1] = y2;

  if (0 > Vect_copy_xy_to_pnts (Points, xarray, yarray, NUM_POINTS))
    G_fatal_error ("Out of memory");
  Vect_write_line (Map, AREA, Points);
}
