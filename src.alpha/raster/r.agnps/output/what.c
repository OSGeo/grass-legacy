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

int what (name)
  char *name;
{
  int fd;
  int row, col;
  CELL *buf;
  char errbuf[1024];
  struct Cell_head window;
  int screen_x, screen_y;
  double east, north;
  int button;
  double D_get_d_north (), D_get_d_south ();
  double D_get_d_east (), D_get_d_west ();
  double D_d_to_u_row (), D_d_to_u_col ();
  char *mapset;
  char temp[128];
  int t, b, l, r;
  int show_mouse ();
  int R_open_driver (), D_get_cur_wind (), G_fatal_error (), D_set_cur_wind ();
  int G_get_window (), D_check_map_window (), G_set_window ();
  int D_get_screen_window (), D_do_conversions (), cell_open ();
  int R_get_location_with_pointer (), G_get_map_row ();
  int R_close_driver (), G_close_cell ();

  R_open_driver ();
  if (D_get_cur_wind (temp))
    G_fatal_error ("No current graphics window");
  if (D_set_cur_wind (temp))
    G_fatal_error ("Current graphics window not available");

  /* Read in the map window associated with window */
  G_get_window (&window);

  if (D_check_map_window (&window))
    G_fatal_error ("Setting graphics window");

  if (G_set_window (&window) == -1)
    G_fatal_error ("Can't set current graphics window");

  /* Determine conversion factors */
  if (D_get_screen_window (&t, &b, &l, &r))
    G_fatal_error ("Getting graphics window coordinates");
  if (D_do_conversions (&window, t, b, l, r))
    G_fatal_error ("Error in calculating conversions");




  buf = G_allocate_cell_buf ();

  screen_x = ((int) D_get_d_west () + (int) D_get_d_east ()) / 2;
  screen_y = ((int) D_get_d_north () + (int) D_get_d_south ()) / 2;


  mapset = G_find_file ("cell", name, "");
  if (!mapset)
  {
    sprintf (errbuf, "Cell Number Map mapset not found to open");
    G_fatal_error (errbuf);
  }

  fd = cell_open (name, mapset);

  show_mouse ();
  R_get_location_with_pointer (&screen_x, &screen_y, &button);
  east = D_d_to_u_col ((double) screen_x);
  north = D_d_to_u_row ((double) screen_y);
  row = (window.north - north) / window.ns_res;
  col = (east - window.west) / window.ew_res;
  if (G_get_map_row (fd, buf, row) < 0)
    exit (0);

  R_close_driver ();

  G_close_cell (fd);

  return (buf[col]);
}


#include <stdio.h>

int show_mouse ()
{
  fprintf (stderr, "\n");
  fprintf (stderr, "Button\n");
  fprintf (stderr, 
      " Left:  Point to a cell on the selected window to view the output\n");
  return 0;
}
