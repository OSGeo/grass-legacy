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
#include "window_management.h"

int usr_avg_stats (window)
  struct Cell_head window;
{
  int screen_x, screen_y;
  int screen_x1, screen_y1;
  int screen_x2, screen_y2;
  double ux1, uy1;
  double ux2, uy2;
  double north, south, east, west;
  double D_u_to_d_row (), D_u_to_d_col ();
  double D_get_d_south ();
  int t;
  int top, bot, left, rite;
  int button;
  int cur_screen_x, cur_screen_y;
  double D_d_to_u_col ();
  double D_d_to_u_row ();
  double D_get_u_west ();
  double D_get_u_south ();
  double D_get_d_west ();
  int R_open_driver (), D_get_screen_window (), G_fatal_error ();
  int D_do_conversions ();
  int get_map_top (), get_map_left ();
  int G_clear_screen (), R_get_location_with_box ();
  int R_standard_color (), R_close_driver ();
  int D_translate_color (), R_move_abs (), R_cont_abs ();

  R_open_driver ();

  /* Determine conversion factors */
  if (D_get_screen_window (&top, &bot, &left, &rite))
    G_fatal_error ("Getting graphics window coordinates");

  if (D_do_conversions (&window, top, bot, left, rite))
    G_fatal_error ("Error in calculating conversions");

  /*
   * D_get_screen_window(&top, &bot, &left, &rite);
   */

  screen_y = get_map_top ();
  screen_x = get_map_left ();

  G_clear_screen ();
  fprintf (stderr, "Buttons:\n");
  fprintf (stderr, "Left:   Establish a corner\n");
  fprintf (stderr, "Right:  Accept window\n");

  cur_screen_x = (int) D_get_d_west ();
  cur_screen_y = (int) D_get_d_south ();
  screen_x = cur_screen_x + 10;
  screen_y = cur_screen_y + 10;
  ux1 = (int) D_get_u_west ();
  uy1 = (int) D_get_u_south ();

  do
  {
    R_get_location_with_box (cur_screen_x, cur_screen_y,
			     &screen_x, &screen_y, &button);
    switch (button & 0x0f)
    {
    case 1:
      cur_screen_x = screen_x;
      cur_screen_y = screen_y;
      ux2 = ux1 = (int) D_d_to_u_col ((double) screen_x);
      uy2 = uy1 = (int) D_d_to_u_row ((double) screen_y);
      break;
    case 3:
      ux2 = (int) D_d_to_u_col ((double) screen_x);
      uy2 = (int) D_d_to_u_row ((double) screen_y);
      break;
    }
  } while (button != 3);


  north = uy1 > uy2 ? uy1 : uy2;
  south = uy1 < uy2 ? uy1 : uy2;
  west = ux1 < ux2 ? ux1 : ux2;
  east = ux1 > ux2 ? ux1 : ux2;

  /* make sure all the four sides lie within the current window */

  if (north > window.north)
    north = window.north;
  if (south < window.south)
    south = window.south;
  if (east > window.east)
    east = window.east;
  if (west < window.west)
    west = window.west;


  t = (window.north - north) / window.ns_res;
  row1 = t;
  north = window.north - (t) * window.ns_res;

  t = (window.north - south) / window.ns_res;
  row2 = t + 1;
  /* row2 can't exceed the # of rows in the window */
  if (row2 > window.rows)
    row2 = window.rows;
  south = window.north - (row2) * window.ns_res;

  t = (east - window.west) / window.ew_res;
  col2 = t + 1;
  /* col2 can't exceed the # of cols in the window */
  if (col2 > window.cols)
    col2 = window.cols;
  east = window.west + (col2) * window.ew_res;

  t = (west - window.west) / window.ew_res;
  col1 = t;
  west = window.west + (t) * window.ew_res;

  window.north = north;
  window.south = south;
  window.east = east;
  window.west = west;

  /* draw a while around the selected box */

  R_standard_color (D_translate_color ("blue"));

  screen_x1 = (int) D_u_to_d_col ((double) window.east);
  screen_y1 = (int) D_u_to_d_row ((double) window.north);

  screen_x2 = (int) D_u_to_d_col ((double) window.west);
  screen_y2 = (int) D_u_to_d_row ((double) window.south);

  screen_x1 = screen_x1 < left ? left : screen_x1;
  screen_y1 = screen_y1 < top ? top : screen_y1;
  screen_x2 = screen_x2 > rite ? rite : screen_x2;
  screen_y2 = screen_y2 > bot ? bot : screen_y2;

  R_move_abs (screen_x1, screen_y1);
  R_cont_abs (screen_x2, screen_y1);
  R_cont_abs (screen_x2, screen_y2);
  R_cont_abs (screen_x1, screen_y2);
  R_cont_abs (screen_x1, screen_y1);

  R_close_driver ();
  return 0;
}

int compute_avg_stats (k)
  int k;
{
  int fd, count;
  int i, j;
  CELL *buf;
  int cell_open ();
  int G_get_map_row ();
  int G_close_cell ();


  fd = cell_open (cell_num_map->p, this_mapset);

  buf = G_allocate_cell_buf ();

  count = 0;

  /* initialize the no_cells[k] +1 array */
  cell_sediment[k][no_cells[k] + 1].cell_erosion[5] = 0.0;
  cell_sediment[k][no_cells[k] + 1].gen_above[5] = 0.0;
  cell_sediment[k][no_cells[k] + 1].within[5] = 0.0;
  cell_sediment[k][no_cells[k] + 1].yield[5] = 0.0;
  cell_sediment[k][no_cells[k] + 1].deposition[5] = 0;

  cell_runoff[k][no_cells[k] + 1].dr_area = 0;
  cell_runoff[k][no_cells[k] + 1].overland_ro = 0.0;
  cell_runoff[k][no_cells[k] + 1].us_ro = 0.0;
  cell_runoff[k][no_cells[k] + 1].peak_us = 0;
  cell_runoff[k][no_cells[k] + 1].ds_ro = 0.0;
  cell_runoff[k][no_cells[k] + 1].peak_ds = 0;

  nut_anlys[k][no_cells[k] + 1].N_sed_within = 0.0;
  nut_anlys[k][no_cells[k] + 1].N_sed_outlet = 0.0;
  nut_anlys[k][no_cells[k] + 1].N_sol_within = 0.0;
  nut_anlys[k][no_cells[k] + 1].N_sol_outlet = 0.0;
  nut_anlys[k][no_cells[k] + 1].N_conc = 0;

  nut_anlys[k][no_cells[k] + 1].P_sed_within = 0.0;
  nut_anlys[k][no_cells[k] + 1].P_sed_outlet = 0.0;
  nut_anlys[k][no_cells[k] + 1].P_sol_within = 0.0;
  nut_anlys[k][no_cells[k] + 1].P_sol_outlet = 0.0;
  nut_anlys[k][no_cells[k] + 1].P_conc = 0;
  nut_anlys[k][no_cells[k] + 1].COD_sol_within = 0.0;
  nut_anlys[k][no_cells[k] + 1].COD_sol_outlet = 0.0;
  nut_anlys[k][no_cells[k] + 1].COD_conc = 0;

  /* initialize the input cells for an selected area */

  ag_inp[k][no_cells[k] + 1].cell_num = 0;
  ag_inp[k][no_cells[k] + 1].rcell_num = 0;
  ag_inp[k][no_cells[k] + 1].cn = 0;
  ag_inp[k][no_cells[k] + 1].slope_pct = 0.0;
  ag_inp[k][no_cells[k] + 1].slope_shape = 0;
  ag_inp[k][no_cells[k] + 1].slope_ln = 0;
  ag_inp[k][no_cells[k] + 1].chnl_slope = 0.0;
  ag_inp[k][no_cells[k] + 1].chnl_side_slope = 0.0;
  ag_inp[k][no_cells[k] + 1].man_n = 0.0;
  ag_inp[k][no_cells[k] + 1].k_val = 0.0;
  ag_inp[k][no_cells[k] + 1].c_fac = 0.0;
  ag_inp[k][no_cells[k] + 1].p_fac = 0.0;
  ag_inp[k][no_cells[k] + 1].surf_cond = 0.0;
  ag_inp[k][no_cells[k] + 1].aspect = 0;
  ag_inp[k][no_cells[k] + 1].texture = 0;
  ag_inp[k][no_cells[k] + 1].fert_fac = 0;
  ag_inp[k][no_cells[k] + 1].fert_avl_fac = 0;
  /*
   * ag_inp[k][no_cells[k]+1].feedlot = 0;
   */
  ag_inp[k][no_cells[k] + 1].gully = 0;
  ag_inp[k][no_cells[k] + 1].cod_fac = 0;
  ag_inp[k][no_cells[k] + 1].impd_fac = 0;
  ag_inp[k][no_cells[k] + 1].chnl_ind = 0;



  for (i = row1; i < row2; i++)
  {
    if (G_get_map_row (fd, buf, i) < 0)
      exit (0);
    for (j = col1; j < col2; j++)
    {
      if (buf[j] > 0)
      {
	cell_sediment[k][no_cells[k] + 1].cell_erosion[5] += cell_sediment[k][buf[j] - 1].cell_erosion[5];
	cell_sediment[k][no_cells[k] + 1].gen_above[5] += cell_sediment[k][buf[j] - 1].gen_above[5];
	cell_sediment[k][no_cells[k] + 1].within[5] += cell_sediment[k][buf[j] - 1].within[5];
	cell_sediment[k][no_cells[k] + 1].yield[5] += cell_sediment[k][buf[j] - 1].yield[5];
	cell_sediment[k][no_cells[k] + 1].deposition[5] += cell_sediment[k][buf[j] - 1].deposition[5];

	cell_runoff[k][no_cells[k] + 1].dr_area += cell_runoff[k][buf[j] - 1].dr_area;
	cell_runoff[k][no_cells[k] + 1].overland_ro += cell_runoff[k][buf[j] - 1].overland_ro;
	cell_runoff[k][no_cells[k] + 1].us_ro += cell_runoff[k][buf[j] - 1].us_ro;
	cell_runoff[k][no_cells[k] + 1].peak_us += cell_runoff[k][buf[j] - 1].peak_us;
	cell_runoff[k][no_cells[k] + 1].ds_ro += cell_runoff[k][buf[j] - 1].ds_ro;
	cell_runoff[k][no_cells[k] + 1].peak_ds += cell_runoff[k][buf[j] - 1].peak_ds;

	nut_anlys[k][no_cells[k] + 1].N_sed_within += nut_anlys[k][buf[j] - 1].N_sed_within;
	nut_anlys[k][no_cells[k] + 1].N_sed_outlet += nut_anlys[k][buf[j] - 1].N_sed_outlet;
	nut_anlys[k][no_cells[k] + 1].N_sol_within += nut_anlys[k][buf[j] - 1].N_sol_within;
	nut_anlys[k][no_cells[k] + 1].N_sol_outlet += nut_anlys[k][buf[j] - 1].N_sol_outlet;
	nut_anlys[k][no_cells[k] + 1].N_conc += nut_anlys[k][buf[j] - 1].N_conc;

	nut_anlys[k][no_cells[k] + 1].P_sed_within += nut_anlys[k][buf[j] - 1].P_sed_within;
	nut_anlys[k][no_cells[k] + 1].P_sed_outlet += nut_anlys[k][buf[j] - 1].P_sed_outlet;
	nut_anlys[k][no_cells[k] + 1].P_sol_within += nut_anlys[k][buf[j] - 1].P_sol_within;
	nut_anlys[k][no_cells[k] + 1].P_sol_outlet += nut_anlys[k][buf[j] - 1].P_sol_outlet;
	nut_anlys[k][no_cells[k] + 1].P_conc += nut_anlys[k][buf[j] - 1].P_conc;

	nut_anlys[k][no_cells[k] + 1].COD_sol_within += nut_anlys[k][buf[j] - 1].COD_sol_within;
	nut_anlys[k][no_cells[k] + 1].COD_sol_outlet += nut_anlys[k][buf[j] - 1].COD_sol_outlet;
	nut_anlys[k][no_cells[k] + 1].COD_conc += nut_anlys[k][buf[j] - 1].COD_conc;

	count += 1;

	ag_inp[k][no_cells[k] + 1].cell_num += ag_inp[k][buf[j] - 1].cell_num;
	ag_inp[k][no_cells[k] + 1].rcell_num += ag_inp[k][buf[j] - 1].rcell_num;
	ag_inp[k][no_cells[k] + 1].cn += ag_inp[k][buf[j] - 1].cn;
	ag_inp[k][no_cells[k] + 1].slope_pct += ag_inp[k][buf[j] - 1].slope_pct;
	ag_inp[k][no_cells[k] + 1].slope_shape += ag_inp[k][buf[j] - 1].slope_shape;
	ag_inp[k][no_cells[k] + 1].slope_ln += ag_inp[k][buf[j] - 1].slope_ln;
	ag_inp[k][no_cells[k] + 1].chnl_slope += ag_inp[k][buf[j] - 1].chnl_slope;
	ag_inp[k][no_cells[k] + 1].chnl_side_slope += ag_inp[k][buf[j] - 1].chnl_side_slope;
	ag_inp[k][no_cells[k] + 1].man_n += ag_inp[k][buf[j] - 1].man_n;
	ag_inp[k][no_cells[k] + 1].k_val += ag_inp[k][buf[j] - 1].k_val;
	ag_inp[k][no_cells[k] + 1].c_fac += ag_inp[k][buf[j] - 1].c_fac;
	ag_inp[k][no_cells[k] + 1].p_fac += ag_inp[k][buf[j] - 1].p_fac;
	ag_inp[k][no_cells[k] + 1].surf_cond += ag_inp[k][buf[j] - 1].surf_cond;
	ag_inp[k][no_cells[k] + 1].aspect += ag_inp[k][buf[j] - 1].aspect;
	ag_inp[k][no_cells[k] + 1].texture += ag_inp[k][buf[j] - 1].texture;
	ag_inp[k][no_cells[k] + 1].fert_fac += ag_inp[k][buf[j] - 1].fert_fac;
	ag_inp[k][no_cells[k] + 1].fert_avl_fac += ag_inp[k][buf[j] - 1].fert_avl_fac;
	/*
	 * ag_inp[k][no_cells[k]+1].feedlot +=
	 * ag_inp[k][buf[j]-1].feedlot;
	 */
	ag_inp[k][no_cells[k] + 1].gully += ag_inp[k][buf[j] - 1].gully;
	ag_inp[k][no_cells[k] + 1].cod_fac += ag_inp[k][buf[j] - 1].cod_fac;
	ag_inp[k][no_cells[k] + 1].impd_fac += ag_inp[k][buf[j] - 1].impd_fac;
	ag_inp[k][no_cells[k] + 1].chnl_ind += ag_inp[k][buf[j] - 1].chnl_ind;
      }
    }
  }

  cell_sediment[k][no_cells[k] + 1].cell_erosion[5] = cell_sediment[k][no_cells[k] + 1].cell_erosion[5] / (float) count;
  cell_sediment[k][no_cells[k] + 1].gen_above[5] = cell_sediment[k][no_cells[k] + 1].gen_above[5] / (float) count;
  cell_sediment[k][no_cells[k] + 1].within[5] = cell_sediment[k][no_cells[k] + 1].within[5] / (float) count;
  cell_sediment[k][no_cells[k] + 1].yield[5] = cell_sediment[k][no_cells[k] + 1].yield[5] / (float) count;
  cell_sediment[k][no_cells[k] + 1].deposition[5] = cell_sediment[k][no_cells[k] + 1].deposition[5] / (float) count;

  cell_runoff[k][no_cells[k] + 1].dr_area = cell_runoff[k][no_cells[k] + 1].dr_area / (float) count;
  cell_runoff[k][no_cells[k] + 1].overland_ro = cell_runoff[k][no_cells[k] + 1].overland_ro / (float) count;
  cell_runoff[k][no_cells[k] + 1].us_ro = cell_runoff[k][no_cells[k] + 1].us_ro / (float) count;
  cell_runoff[k][no_cells[k] + 1].peak_us = cell_runoff[k][no_cells[k] + 1].peak_us / (float) count;
  cell_runoff[k][no_cells[k] + 1].ds_ro = cell_runoff[k][no_cells[k] + 1].ds_ro / (float) count;
  cell_runoff[k][no_cells[k] + 1].peak_ds = cell_runoff[k][no_cells[k] + 1].peak_ds / (float) count;

  nut_anlys[k][no_cells[k] + 1].N_sed_within = nut_anlys[k][no_cells[k] + 1].N_sed_within / (float) count;
  nut_anlys[k][no_cells[k] + 1].N_sed_outlet = nut_anlys[k][no_cells[k] + 1].N_sed_outlet / (float) count;
  nut_anlys[k][no_cells[k] + 1].N_sol_within = nut_anlys[k][no_cells[k] + 1].N_sol_within / (float) count;
  nut_anlys[k][no_cells[k] + 1].N_sol_outlet = nut_anlys[k][no_cells[k] + 1].N_sol_outlet / (float) count;
  nut_anlys[k][no_cells[k] + 1].N_conc = nut_anlys[k][no_cells[k] + 1].N_conc / (float) count;

  nut_anlys[k][no_cells[k] + 1].P_sed_within = nut_anlys[k][no_cells[k] + 1].P_sed_within / (float) count;
  nut_anlys[k][no_cells[k] + 1].P_sed_outlet = nut_anlys[k][no_cells[k] + 1].P_sed_outlet / (float) count;
  nut_anlys[k][no_cells[k] + 1].P_sol_within = nut_anlys[k][no_cells[k] + 1].P_sol_within / (float) count;
  nut_anlys[k][no_cells[k] + 1].P_sol_outlet = nut_anlys[k][no_cells[k] + 1].P_sol_outlet / (float) count;
  nut_anlys[k][no_cells[k] + 1].P_conc = nut_anlys[k][no_cells[k] + 1].P_conc / (float) count;
  nut_anlys[k][no_cells[k] + 1].COD_sol_within = nut_anlys[k][no_cells[k] + 1].COD_sol_within / (float) count;
  nut_anlys[k][no_cells[k] + 1].COD_sol_outlet = nut_anlys[k][no_cells[k] + 1].COD_sol_outlet / (float) count;
  nut_anlys[k][no_cells[k] + 1].COD_conc = nut_anlys[k][no_cells[k] + 1].COD_conc / (float) count;

  ag_inp[k][no_cells[k] + 1].cell_num =
    (int) ((float) ag_inp[k][no_cells[k] + 1].cell_num / (float) count);
  ag_inp[k][no_cells[k] + 1].rcell_num =
    (int) ((float) ag_inp[k][no_cells[k] + 1].rcell_num / (float) count);
  ag_inp[k][no_cells[k] + 1].cn =
    (int) ((float) ag_inp[k][no_cells[k] + 1].cn / (float) count);
  ag_inp[k][no_cells[k] + 1].slope_pct =
    ag_inp[k][no_cells[k] + 1].slope_pct / (float) count;
  ag_inp[k][no_cells[k] + 1].slope_shape =
    (int) ((float) ag_inp[k][no_cells[k] + 1].slope_shape / (float) count);
  ag_inp[k][no_cells[k] + 1].slope_ln =
    (int) ((float) ag_inp[k][no_cells[k] + 1].slope_ln / (float) count);
  ag_inp[k][no_cells[k] + 1].chnl_slope =
    ag_inp[k][no_cells[k] + 1].chnl_slope / (float) count;
  ag_inp[k][no_cells[k] + 1].chnl_side_slope =
    ag_inp[k][no_cells[k] + 1].chnl_side_slope / (float) count;
  ag_inp[k][no_cells[k] + 1].man_n =
    ag_inp[k][no_cells[k] + 1].man_n / (float) count;
  ag_inp[k][no_cells[k] + 1].k_val =
    ag_inp[k][no_cells[k] + 1].k_val / (float) count;
  ag_inp[k][no_cells[k] + 1].c_fac =
    ag_inp[k][no_cells[k] + 1].c_fac / (float) count;
  ag_inp[k][no_cells[k] + 1].p_fac =
    ag_inp[k][no_cells[k] + 1].p_fac / (float) count;
  ag_inp[k][no_cells[k] + 1].surf_cond =
    ag_inp[k][no_cells[k] + 1].surf_cond / (float) count;
  ag_inp[k][no_cells[k] + 1].aspect =
    (int) ((float) ag_inp[k][no_cells[k] + 1].aspect / (float) count);
  ag_inp[k][no_cells[k] + 1].texture =
    (int) ((float) ag_inp[k][no_cells[k] + 1].texture / (float) count);
  ag_inp[k][no_cells[k] + 1].fert_fac =
    (int) ((float) ag_inp[k][no_cells[k] + 1].fert_fac / (float) count);
  ag_inp[k][no_cells[k] + 1].fert_avl_fac =
    (int) ((float) ag_inp[k][no_cells[k] + 1].fert_avl_fac / (float) count);
  /*
   * ag_inp[k][no_cells[k]+1].feedlot = (int)
   * ((float)ag_inp[k][no_cells[k]+1].feedlot/(float) count);
   */
  ag_inp[k][no_cells[k] + 1].gully =
    (int) ((float) ag_inp[k][no_cells[k] + 1].gully / (float) count);
  ag_inp[k][no_cells[k] + 1].cod_fac =
    (int) ((float) ag_inp[k][no_cells[k] + 1].cod_fac / (float) count);
  ag_inp[k][no_cells[k] + 1].impd_fac =
    (int) ((float) ag_inp[k][no_cells[k] + 1].impd_fac / (float) count);
  ag_inp[k][no_cells[k] + 1].chnl_ind =
    (int) ((float) ag_inp[k][no_cells[k] + 1].chnl_ind / (float) count);

  G_close_cell (fd);

  /*
   * R_open_driver(); Dchoose(w_name[num_top_row_win+2].window_name);
   * Derase("black"); R_close_driver();
   * 
   * histogram(no_cells[k]+2,choice);
   * 
   * R_open_driver(); Dchoose(w_name[num_top_row_win+1].window_name);
   * Derase("black"); R_close_driver();
   * 
   * in_wind(no_cells[k]+2);
   */

  return 0;
}

/* %W%  %G%  */

#define BLOCKS_PER_COL  32
#define TEXT1_X         .61
#define TEXT1_Y         .03
#define TEXT2_X         .61
#define TEXT2_Y         .06
#define TITL1_X         .01
#define TITL1_Y         .03
#define TITL2_X         .01
#define TITL2_Y         .06
#define WINDOW_PROP_SCREEN_X    .8500
#define WINDOW_PROP_SCREEN_Y    .9373

#include <stdio.h>
#define GETEM	if (notgotem) getem()

static int notgotem = 1;
static int wind_top;
static int wind_bot;
static int wind_rite;
static int wind_left;

int get_wind_bot ()
{
  GETEM;
  return (wind_bot);
}

int get_wind_top ()
{
  GETEM;
  return (wind_top);
}

int get_wind_rite ()
{
  GETEM;
  return (wind_rite);
}

int get_wind_left ()
{
  GETEM;
  return (wind_left);
}

int get_map_bot ()
{
  GETEM;
  return (wind_bot);
}

int get_map_top ()
{
  register float tmp1, tmp2, tmp3;

  GETEM;
  if (notgotem)
  {
    getem ();
    notgotem = 0;
  }
  tmp1 = (float) wind_bot;
  tmp2 = (float) wind_top;
  tmp3 = tmp1 + WINDOW_PROP_SCREEN_Y * (tmp2 - tmp1);
  return ((int) tmp3);
}

int get_map_left ()
{
  register float tmp1, tmp2, tmp3;

  GETEM;
  tmp1 = (float) wind_left;
  tmp2 = (float) wind_rite;
  tmp3 = tmp2 - WINDOW_PROP_SCREEN_X * (tmp2 - tmp1);
  return ((int) tmp3);
}

int get_map_rite ()
{
  GETEM;
  return (wind_rite);
}

int get_wind_y_pos (position)
  float position;
{
  register float tmp1, tmp2, tmp3;

  GETEM;
  tmp1 = (float) wind_top;
  tmp2 = (float) wind_bot;
  tmp3 = tmp1 + position * (tmp2 - tmp1);
  return ((int) tmp3);
}

int get_wind_x_pos (position)
  float position;
{
  register float tmp1, tmp2, tmp3;

  GETEM;
  tmp1 = (float) wind_left;
  tmp2 = (float) wind_rite;
  tmp3 = tmp1 + position * (tmp2 - tmp1);
  return ((int) tmp3);
}

static int
 getem ()
{
  D_get_screen_window (&wind_top, &wind_bot,
		       &wind_left, &wind_rite);
  notgotem = 0;
}
