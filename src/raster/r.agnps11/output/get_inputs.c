
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
#include<stdlib.h>

int get_inputs ()
{
  char buf[512], *mapset;
  int nrows, ncols, ct, i, j, fd;
  CELL *rbuf;
  int get_wshd_inputs (), cell_open (), cell_open_new ();
  int G_gisinit (), G_fatal_error (), G_gets (), G_get_cellhd ();
  int G_put_window (), G_set_window (), G_get_map_row (), G_zero_cell_buf ();
  int G_put_map_row (), G_close_cell (), G_system ();

  G_gisinit ("maps");

  get_wshd_inputs ();

  if (strcmp (wshd_name, "") == 0)
  {
    mapset = G_ask_cell_old ("Watershed map for Masking", wshd_name);

    if (!mapset)
    {
      sprintf (buf, "%s Cell Map mapset not found to open", wshd_name);
      G_fatal_error (buf);
    }
  }
  else
    mapset = get_mapset (wshd_name);

  if (grid_res[cur] == 0)
  {

    do
    {
      printf ("\n\nEnter the cell size in meters --> ");
    } while (!G_gets (buf));

    grid_res[cur] = atoi (buf);
  }

  G_get_cellhd (wshd_name, mapset, &orig_window);

  orig_window.ns_res = (double) grid_res[cur];
  orig_window.ew_res = (double) grid_res[cur];
  orig_window.rows = (int) ((orig_window.north - orig_window.south) / orig_window.ns_res);
  orig_window.cols = (int) ((orig_window.east - orig_window.west) / orig_window.ew_res);
  orig_window.south = orig_window.north - (orig_window.rows * orig_window.ns_res);
  orig_window.west = orig_window.east - (orig_window.cols * orig_window.ew_res);

  G_put_window (orig_window);

  G_set_window (&orig_window);
  nrows = orig_window.rows;
  ncols = orig_window.cols;

  /* allocate memory for the cell buf */
  rbuf = G_allocate_cell_buf ();

  fd = cell_open (wshd_name, mapset);

  cell_num_map = (MAPS *) emalloc ((unsigned) sizeof (MAPS));
  cell_num_map->p = emalloc ((unsigned) (64));

  if (strcmp (file_name, "") != 0)
    sprintf (cell_num_map->p, "%s_cell_num", file_name);
  else
  {
    printf ("\nEnter the ASCII AGNPS file name without extension --> ");
    while (!G_gets (file_name));
    sprintf (cell_num_map->p, "%s_cell_num", file_name);
  }


  cell_num_map->fd = cell_open_new (cell_num_map->p);

  cell_num_map->rbuf = G_allocate_cell_buf ();

  ct = 1;

  for (i = 0; i < nrows; i++)
  {
    G_get_map_row (fd, rbuf, i);
    G_zero_cell_buf (cell_num_map->rbuf);

    for (j = 0; j < ncols; j++)
    {
      if (rbuf[j] > 0)
      {
	cell_num_map->rbuf[j] = ct;
	ct++;
      }
    }

    G_put_map_row (cell_num_map->fd, cell_num_map->rbuf);
  }

  /*
   * no_cells[cur] = ct - 1;
   */
  no_cell = ct - 1;

  G_close_cell (cell_num_map->fd);
  G_close_cell (fd);

  sprintf (buf, "g.copy rast=%s,MASK", cell_num_map->p);
  G_system (buf);

  if (strcmp (wshd_aspect, "") == 0)
  {
    mapset = G_ask_cell_old ("Aspect map for display ", wshd_aspect);

    if (!mapset)
    {
      sprintf (buf, "%s Cell Map mapset not found to open", wshd_aspect);
      G_fatal_error (buf);
    }
  }
  else
    mapset = get_mapset (wshd_aspect);
  return 0;
}


int get_wshd_inputs ()
{

  char buf[512];
  int V_clear (), V_line (), V_call (), V_ques ();

  buf[0] = (char) NULL;

  V_clear ();
  V_line (1, "       AGNPS/GRASS Output Interface (Visualization) Tool");
  V_line (3, "                    Version 1.1 (4/30/92)");
  V_line (5, "               R. Srinivasan and B.A. Engel");
  V_line (7, "                      Phone: 317-494-1198 ");
  V_line (9, "                email: engelb@ecn.purdue.edu");
  V_line (11, "             Agricultural Engineering Department ");
  V_line (12, "                     Purdue University");
  V_line (13, "                 West Lafayette, IN 47907");
  V_line (15, "     (c)Copyright, 1992 Purdue Research Foundation, West");
  V_line (16, "     Lafayette, Indiana 47907. All Rights Reserved. Unless");
  V_line (17, "     permission is granted, this material shall not be");
  V_line (18, "     copied, reproduced or coded for reproduction by any");
  V_line (19, "     electrical, mechanical or chemical processes,  or");
  V_line (20, "     combinations thereof, now known or later developed.");

  if (!V_call ())
    exit (1);


  for (;;)
  {
    V_clear ();
    V_line (3, "Visualization Tool  Input ");
    V_line (7, "1. Enter Watershed Map name ");
    V_line (9, "2. Enter the cell size in meters ");
    V_line (11, "3. Enter Aspect Map name");
    V_line (13, "4. Enter the ASCII AGNPS file name without extension");
    V_line (14, "	(proper extension is assumed as .dat and .nps)");
    V_ques (wshd_name, 's', 7, 55, 15);
    V_ques (&grid_res[cur], 'i', 9, 55, 5);
    V_ques (wshd_aspect, 's', 11, 55, 15);
    V_ques (file_name, 's', 13, 55, 15);

    if (!V_call ())
      exit (1);


    V_clear ();
    /*
     * V_line(3, "\tWould you like to modify the Visualization Tool Input
     * (y/n) "); V_ques(buf,'s',3,68,1);
     * 
     * if(!V_call()) exit(1);
     * 
     * if(strcmp(buf,"y") == 0 || strcmp(buf, "Y") == 0) continue; else
     * break;
     */
    break;
  }
  return 0;
}
