/* %W%  %G%  */

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

int wshd_wind (win_name, name)
  char *name, *win_name;
{
  int screen_x1, screen_y1;
  int screen_x2, screen_y2;
  double D_u_to_d_row (), D_u_to_d_col ();
  char *mapset;
  char buf[512];
  int t, b, l, r, win_label ();
  struct Cell_head window;
  int G__get_window (), R_open_driver (), Dchoose (), Derase ();
  int G_set_window (), G_fatal_error (), D_check_map_window (); 
  int D_get_screen_window (), D_do_conversions ();
  int Dcell (), R_standard_color (), D_translate_color ();
  int R_move_abs (), R_cont_abs (), R_close_driver ();


  G__get_window (&window, "", "WIND", G_mapset ());

  R_open_driver ();

  Dchoose (win_name);
  Derase ("black");

  if (G_set_window (&orig_window) == -1)
    G_fatal_error ("Can't set current graphics window");

  if (D_check_map_window (&orig_window))
    G_fatal_error ("Setting graphics window");

  if (G_set_window (&orig_window) == -1)
    G_fatal_error ("Can't set current graphics window");

  /* Determine conversion factors */
  if (D_get_screen_window (&t, &b, &l, &r))
    G_fatal_error ("Getting graphics window coordinates");
  if (D_do_conversions (&orig_window, t, b, l, r))
    G_fatal_error ("Error in calculating conversions");

  D_get_screen_window (&t, &b, &l, &r);

  mapset = G_find_file ("cell", name, "");
  if (!mapset)
  {
    sprintf (buf, "Cell Number Map mapset not found to open");
    G_fatal_error (buf);
  }

  Dcell (name, mapset, 0);

  R_standard_color (D_translate_color ("white"));

  screen_x1 = (int) D_u_to_d_col ((double) window.east);
  screen_y1 = (int) D_u_to_d_row ((double) window.north);

  screen_x2 = (int) D_u_to_d_col ((double) window.west);
  screen_y2 = (int) D_u_to_d_row ((double) window.south);

  screen_x1 = screen_x1 < l ? l : screen_x1;
  screen_y1 = screen_y1 < t ? t : screen_y1;
  screen_x2 = screen_x2 > r ? r : screen_x2;
  screen_y2 = screen_y2 > b ? b : screen_y2;

  R_move_abs (screen_x1, screen_y1);
  R_cont_abs (screen_x2, screen_y1);
  R_cont_abs (screen_x2, screen_y2);
  R_cont_abs (screen_x1, screen_y2);
  R_cont_abs (screen_x1, screen_y1);

  R_close_driver ();
  win_label ("Watershed with current viewing area");
  return 0;
}
