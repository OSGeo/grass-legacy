
/*---------------------------------------------------------*
 *               AGNPS/GRASS Interface Project             *
 *  Developed in the Agriculture Engineering Department    *
 *                at Purdue University                     *
 *                        by                               *
 *         Raghavan Srinivasan and Bernard Engel           *
 *                                                         *
 *   (c)Copyright, 1992 Purdue Research Foundation, West   *
 *   Lafayette, Indiana 47907. All Rights Reserved. Unless *
 *   permission is granted, this material shall not be     *
 *   copied, reproduced or coded for reproduction by any   *
 *   electrical, mechanical or chemical processes,  or     *
 *   combinations thereof, now known or later developed.   *
 *---------------------------------------------------------*/

#include "map_gen.h"
#define	left	5
#define	bot	5

int draw_scale (name, max, min)
  char *name;
  float max, min;
{

  int t, b, l, r;
  int i, box_size, cell_val;
  float factor;
  float mean, inc;
  struct Colors colors;
  char buf[512], *mapset;
  int G_fatal_error (), G_read_colors (), R_open_driver (), D_set_colors ();
  int D_get_screen_window (), D_color (), R_move_abs (), R_box_rel ();
  int R_text_size (), R_standard_color (), D_translate_color (), R_set_window ();
  int R_text (), R_flush (), R_close_driver (), G_free_colors (), R_font ();


  mapset = G_find_file ("cell", name, "");
  if (!mapset)
  {
    sprintf (buf, "%s Cell Map mapset not found to open", name);
    G_fatal_error (buf);
  }

  G_read_colors (name, mapset, &colors);


  mean = (max + min) / 2;
  inc = (mean - min) / 5.0;


  R_open_driver ();

  cell_val = D_set_colors (&colors);

  D_get_screen_window (&t, &b, &l, &r);
  t = b - bot - 35;
  b = b - bot;
  l = l + left;

  box_size = (int) (r - l - 5) / 11;

  cell_val = (int) (min * sig_fac + 0.5);
  D_color ((CELL) cell_val, &colors);
  R_move_abs (l, b);
  R_box_rel (box_size, -box_size);

  R_text_size (7, 15);
  R_font ("romans");
  R_standard_color (D_translate_color ("yellow"));
  sprintf (buf, "%4.2f", min);
  R_move_abs (l, t);
  R_set_window (t - 25, b - 30, r = l, r = r + box_size);
  R_text (buf);
  R_flush ();

  for (i = 0; i < 4; i++)
  {
    factor = min + (i + 1) * inc;
    cell_val = (int) ((factor * sig_fac) + 0.5);
    D_color ((CELL) cell_val, &colors);
    R_move_abs (l + (i + 1) * box_size, b);
    R_box_rel (box_size, -box_size);
    R_flush ();
    if (i == 1 || i == 3)
    {
      R_standard_color (D_translate_color ("yellow"));
      sprintf (buf, "%4.2f", factor);
      R_move_abs (l + (i + 1) * box_size, t);
      R_set_window (t - 25, b - 30, r = l + (i + 1) * box_size, r = r + box_size);
      R_text (buf);
      R_flush ();
    }
  }


  cell_val = (int) ((mean * sig_fac) + 0.5);
  D_color ((CELL) cell_val, &colors);
  R_move_abs (l + 5 * box_size, b);
  R_box_rel (box_size, -box_size);
  R_flush ();

  for (i = 0; i < 4; i++)
  {
    factor = mean + (i + 1) * inc;
    cell_val = (int) ((factor * sig_fac) + 0.5);
    D_color ((CELL) cell_val, &colors);
    R_move_abs (l + (i + 6) * box_size, b);
    R_box_rel (box_size, -box_size);
    R_flush ();
    if (i == 0 || i == 2)
    {
      R_standard_color (D_translate_color ("yellow"));
      sprintf (buf, "%4.2f", factor);
      R_move_abs (l + (i + 6) * box_size, t);
      R_set_window (t - 25, b - 30, r = l + (i + 6) * box_size, r = r + box_size);
      R_text (buf);
      R_flush ();
    }
  }

  cell_val = (int) ((max * sig_fac) + 0.5);
  D_color ((CELL) cell_val, &colors);
  R_move_abs (l + 10 * box_size, b);
  R_box_rel (box_size, -box_size);
  R_standard_color (D_translate_color ("yellow"));
  sprintf (buf, "%4.2f", max);
  R_move_abs (l + 10 * box_size, t);
  R_set_window (t - 25, b - 30, r = l + 10 * box_size, r = r + box_size);
  R_text (buf);
  R_flush ();

  R_close_driver ();

  G_free_colors (&colors);
  return 0;
}
