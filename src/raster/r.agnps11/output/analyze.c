
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

/*
 * Modified on 2 June 1991 Raghavan Srinivasan (srin@ecn.purdue.edu)
 * 
 * analyze()
 * 
 * To analyze two different simulations of same watershed and same
 * resolution. This routine contains loop for various options in the
 * sub-menu
 */

#include "map_gen.h"
#include "window_management.h"

int analyze ()
{
  int t, b, l, r;
  int i, option, inc_ew_top, inc_ew_bot, left;
  struct Cell_head window;
  char temp[128];
  int cell_in_out_option (), cell_value;
  int choice;
  float temp1, temp2;
  int choice1, option1;
  char buf[128];
  extern void working_sngl ();
  int read_input (), read_output (), cr_so_ro_maps ();
  int cr_NPCOD_maps (), get_mx_mn_inputs ();
  int G_yes (), G_clear_screen (), G_warning (), G_system ();
  int R_open_driver (), D_get_cur_wind (), G_fatal_error (), D_set_cur_wind ();
  int G_put_window (), G_set_window (), D_check_map_window ();
  int D_get_screen_window (), D_new_window (), G_gets ();
  int what (), usr_avg_stats (), compute_avg_stats (), wshd_wind ();
  int display_map (), analyze_data_run (), wshd_summary (), user_view ();
  int cr_usr_maps (), dis_usr_cr_maps (), display_where (), usr_display_opt ();
  int usr_display (), cdf_var_opt (), cdf_incr_opt (), cdfx (), cdfy ();
  int win_label (), line (), analyze_output_sel ();
  int analyze_show_maps (), analyze_menu (), cell_input_option ();
  int analyze_data_opt (), in_wind (), histogram (), what ();
  int usr_avg_stats (), compute_avg_stats (), wshd_wind ();
  int R_close_driver (), G__get_window (), Dchoose (), Derase ();

  ANALYSIS = YES;

  if (ANALYSIS_DATA == YES)
    if (G_yes ("Would you like to use the selected data sets for analyzis", 1))
      goto START;

  G_clear_screen ();
  printf ("\n\n Enter the ASCII AGNPS file name without extension --> ");
  scanf ("%s", temp);

  if (strcmp (temp, file_name) == 0)
  {
    ANALYSIS = NO;
    G_warning ("File name is same as the current run");
    return 0;
  }

  working_sngl ();

  read_input (temp, prev);

  if (ANALYSIS == NO)
    return 0;
  read_output (temp, prev);
  if (ANALYSIS == NO)
    return 0;
  cr_so_ro_maps (prev);
  cr_NPCOD_maps (prev);

  get_mx_mn_inputs (prev);

  ANALYSIS_DATA = YES;

START:
  num_windows = 6;
  num_top_row_win = 4;
  num_bot_row_win = 2;

  G_system ("d.frame -e");

  R_open_driver ();

  /*
   * b = R_screen_bot(); t = R_screen_top(); l = R_screen_left(); r =
   * R_screen_rite();
   */


  if (D_get_cur_wind (temp))
    G_fatal_error ("No current graphics window");

  if (D_set_cur_wind (temp))
    G_fatal_error ("Current graphics window not available");

  /* Read in the map window associated with window */
  /*
   * G_get_window(&window) ;
   * 
   * if (D_check_map_window(&window)) G_fatal_error("Setting graphics
   * window") ;
   */

  G_put_window (&orig_window);

  if (G_set_window (&orig_window) == -1)
    G_fatal_error ("Can't set current graphics window");
  if (D_check_map_window (&orig_window))
    G_fatal_error ("Setting graphics window");


  D_get_screen_window (&t, &b, &l, &r);

  inc_ew_top = (int) (r - l) / num_top_row_win;
  inc_ew_bot = (int) (r - l) / (num_bot_row_win + 2);

  left = l;


  for (i = 0; i < num_windows; i++)
    sprintf (w_name[i].window_name, "wind%d", i);

  for (i = 0; i < num_top_row_win; i++)
  {
    D_new_window (w_name[i].window_name, t + 1, (int) (((b - t) / 2) - 1), l + 1, l = l + inc_ew_top - 1);
  }

  for (i = num_top_row_win; i < num_windows; i++)
    D_new_window (w_name[i].window_name, (int) (((b - t) / 2) + 1), b - 1, left + 1, left = left + inc_ew_bot - 1);

  R_close_driver ();

  SED_ANALYSIS = YES;
  RO_ANALYSIS = NO;
  N_SED_ANALYSIS = NO;
  P_SED_ANALYSIS = NO;
  N_RO_ANALYSIS = NO;
  P_RO_ANALYSIS = NO;
  COD_ANALYSIS = NO;

  choice = soil_loss;

  wshd_view = NO;
  analyze_show_maps (orig_window, choice);

  for (;;)
  {
    option = analyze_menu ();
    if (option == 4 && wshd_view == NO)
      wshd_view = YES;
    else if (option == 4 && wshd_view == YES)
      wshd_view = NO;

    switch (option)
    {
    case 1:
      printf ("Please choose a window to zoom\n");
      G_system ("d.frame -s");
      G_system ("d.rast.zoom");
      /*
       * make sure the new window is read and set before updateing the
       * screen and maps
       */
      G__get_window (&window, "", "WIND", G_mapset ());
      if (G_set_window (&window) == -1)
	G_fatal_error ("Can't set current graphics window");
      working_sngl ();
      analyze_show_maps (window, choice);
      break;

    case 2:
      option = cell_input_option ();
      switch (option)
      {
      case 1:
	for (;;)
	{
	  G_clear_screen ();
	  printf ("\n\nPlease enter the cell number between 1 - %d --> ", no_cells[cur]);
	  scanf ("%d", &cell_value);
	  if (cell_value > 0 && cell_value <= no_cells[cur])
	    break;
	}

	option = analyze_data_opt ();

	working_sngl ();

	R_open_driver ();
	Dchoose (w_name[num_top_row_win].window_name);
	Derase ("black");
	R_close_driver ();

	if (option == 1)
	  in_wind (cell_value, cur);
	else
	  histogram (cell_value, choice, cur);

	R_open_driver ();
	Dchoose (w_name[num_top_row_win + 1].window_name);
	Derase ("black");
	R_close_driver ();
	if (option == 1)
	  in_wind (cell_value, prev);
	else
	  histogram (cell_value, choice, prev);
	break;
      case 2:
	printf ("Please choose a window to view a cell output\n");
	G_system ("d.frame -s");
	for (;;)
	{
	  cell_value = what (cell_num_map->p);
	  if (cell_value > 0 && cell_value <= no_cells[cur])
	    break;
	  else
	    G_warning ("Please choose a cell from the displayed map of chosen window");
	}

	option = analyze_data_opt ();
	working_sngl ();

	R_open_driver ();
	Dchoose (w_name[num_top_row_win].window_name);
	Derase ("black");
	R_close_driver ();

	if (option == 1)
	  in_wind (cell_value, cur);
	else
	  histogram (cell_value, choice, cur);

	R_open_driver ();
	Dchoose (w_name[num_top_row_win + 1].window_name);
	Derase ("black");
	R_close_driver ();

	if (option == 1)
	  in_wind (cell_value, prev);
	else
	  histogram (cell_value, choice, prev);

	break;
      }
      break;

    case 3:
      option = analyze_data_opt ();

      printf ("Please choose a window to view a cell output\n");
      G_system ("d.frame -s");

      working_sngl ();

      G__get_window (&window, "", "WIND", G_mapset ());
      if (G_set_window (&window) == -1)
	G_fatal_error ("Can't set current graphics window");

      usr_avg_stats (window);
      compute_avg_stats (cur);

      R_open_driver ();
      Dchoose (w_name[num_top_row_win].window_name);
      Derase ("black");
      R_close_driver ();

      if (option == 1)
	in_wind (no_cells[cur] + 2, cur);
      else
	histogram (no_cells[cur] + 2, choice, cur);

      compute_avg_stats (prev);

      R_open_driver ();
      Dchoose (w_name[num_top_row_win + 1].window_name);
      Derase ("black");
      R_close_driver ();

      if (option == 1)
	in_wind (no_cells[prev] + 2, prev);
      else
	histogram (no_cells[prev] + 2, choice, prev);

      break;
    case 4:
      if (wshd_view == YES)
	wshd_wind (w_name[num_top_row_win - 1].window_name, cell_num_map->p);
      else if (wshd_view == NO)
      {
	G__get_window (&window, "", "WIND", G_mapset ());
	display_map (w_name[num_top_row_win - 1].window_name, window, wshd_aspect, "Flow direction map", 5);
      }
      break;

    case 5:
      option = analyze_data_run ();
      wshd_summary (option - 1);
      break;


    case 6:
      /*
       * save_maps(analysis);
       */
      user_view (analysis, cur);
      cr_usr_maps (orig_max, orig_min, choice, cur);
      cr_usr_maps (orig_max, orig_min, choice, prev);
      dis_usr_cr_maps (orig_max, orig_min, analysis);
      usr_modified = YES;
      break;
    case 7:
      option = display_where ();
      if (option == 1)
      {
	printf ("Please choose a window to display map\n");
	G_system ("d.frame -s");
	option = usr_display_opt ();
	working_sngl ();
	usr_display (option);
      }
      else
      {
	printf ("Create a new window \n");
	G_system ("d.frame -c");
	R_open_driver ();
	Derase ("black");
	R_close_driver ();
	option = usr_display_opt ();
	working_sngl ();
	usr_display (option);
      }
      break;
    case 8:
      printf ("Please choose a window to display the stats\n");
      G_system ("d.frame -s");
      G_system ("d.erase");
      option = cdf_var_opt ();
      option1 = analyze_data_run ();
      if (!G_yes ("Would you like to keep the previous X-axis intervals", 0))
      {
	choice1 = cdf_incr_opt ();
	cdfx (option, choice1, option1 - 1);
      }
      cdfy (option, option1 - 1);
      printf ("Please Enter a legend for the graph -->");
      while (!G_gets (buf));
      printf ("\n\nPlease Enter a label for X-AXIS -->");
      while (!G_gets (temp));
      win_label (buf);
      R_open_driver ();
      line (temp, YES);		/* YES draw Y-axis tics and units */
      R_close_driver ();
      temp1 = Y[0];
      temp2 = Y[1];
      for (i = 1; i < no_X - 1; i++)
      {
	Y[i] = temp2 - temp1;
	temp1 = temp2;
	temp2 = Y[i + 1];
      }
      Y[no_X - 1] = Y[no_X - 1] - temp1;
      R_open_driver ();
      line (temp, NO);
      R_close_driver ();
      break;

    case 9:
      working_sngl ();
      goto START;
      break;
    case 10:
      working_sngl ();
      G_system ("myg");
      break;
    case 11:
      choice = analyze_output_sel ();
      /*
       * make sure the new window is read and set before updateing the
       * screen and maps
       */
      G__get_window (&window, "", "WIND", G_mapset ());
      if (G_set_window (&window) == -1)
	G_fatal_error ("Can't set current graphics window");
      working_sngl ();
      analyze_show_maps (window, choice);
      break;
    case 12:
      /*
       * if(usr_modified == YES) save_maps(analysis);
       */
      ANALYSIS = NO;
      return 0;
    }
  }
  return 0;
}


int analyze_show_maps (window, choice)
  struct Cell_head window;
  int choice;
{
  char buf[512];
  int R_open_driver (), Dchoose (), Derase (), R_close_driver ();
  int display_map (), draw_scale (), histogram ();


  if (ANALYSIS && SED_ANALYSIS)
  {

    display_map (w_name[0].window_name, window, sed_in, "Erosion in tons(current)", 1);
    draw_scale (sed_in, gen_above_max[cur], gen_above_min[cur]);
    sprintf (buf, "%s_prev", sed_in);
    display_map (w_name[1].window_name, window, buf, "Erosion in tons(selected)", 1);
    draw_scale (buf, gen_above_max[prev], gen_above_min[prev]);

    display_map (w_name[2].window_name, window, sed_gen, "Deposition in tons(current)", 2);
    draw_scale (sed_gen, within_max[cur], within_min[cur]);
    sprintf (buf, "%s_prev", sed_gen);
    display_map (w_name[3].window_name, window, buf, "Deposition in tons(selected)", 2);
    draw_scale (buf, within_max[prev], within_min[prev]);
  }

  else if (ANALYSIS && RO_ANALYSIS)
  {

    display_map (w_name[0].window_name, window, ro_gen, "Runoff generated in inches(C)", 2);
    draw_scale (ro_gen, ro_gen_max[cur], ro_gen_min[cur]);
    sprintf (buf, "%s_prev", ro_gen);
    display_map (w_name[1].window_name, window, buf, "Runoff generated in inches(S)", 2);
    draw_scale (buf, ro_gen_max[prev], ro_gen_min[prev]);
    display_map (w_name[2].window_name, window, ro_ds, "Runoff to Downstream in inches(C)", 3);
    draw_scale (ro_ds, ro_ds_max[cur], ro_ds_min[cur]);
    sprintf (buf, "%s_prev", ro_ds);
    display_map (w_name[3].window_name, window, buf, "Runoff to Downstream in inches(S)", 3);
    draw_scale (buf, ro_ds_max[prev], ro_ds_min[prev]);
  }

  else if (ANALYSIS && N_SED_ANALYSIS)
  {
    display_map (w_name[0].window_name, window, N_sed_in, "Total N in Sediment generated in lbs per ac(C)", 1);
    draw_scale (N_sed_in, N_sed_in_max[cur], N_sed_in_min[cur]);
    sprintf (buf, "%s_prev", N_sed_in);
    display_map (w_name[1].window_name, window, buf, "Total N in Sediment generated in lbs per ac(S)", 1);
    draw_scale (buf, N_sed_in_max[prev], N_sed_in_min[prev]);
    display_map (w_name[2].window_name, window, N_sed_out, "Total N in Sediment leaving in lbs per ac(C)", 2);
    draw_scale (N_sed_out, N_sed_out_max[cur], N_sed_out_min[cur]);
    sprintf (buf, "%s_prev", N_sed_out);
    display_map (w_name[3].window_name, window, buf, "Total N in Sediment leaving in lbs per ac(S)", 2);
    draw_scale (buf, N_sed_out_max[prev], N_sed_out_min[prev]);
  }
  else if (ANALYSIS && N_RO_ANALYSIS)
  {
    display_map (w_name[0].window_name, window, N_ro_in, "Total N in Runoff generated in lbs per ac(C)", 1);
    draw_scale (N_ro_in, N_ro_in_max[cur], N_ro_in_min[cur]);
    sprintf (buf, "%s_prev", N_ro_in);
    display_map (w_name[1].window_name, window, buf, "Total N in Runoff generated in lbs per ac(S)", 1);
    draw_scale (buf, N_ro_in_max[prev], N_ro_in_min[prev]);
    display_map (w_name[2].window_name, window, N_ro_out, "Total N in Runoff leaving in lbs per ac(C)", 2);
    draw_scale (N_ro_out, N_ro_out_max[cur], N_ro_out_min[cur]);
    sprintf (buf, "%s_prev", N_ro_out);
    display_map (w_name[3].window_name, window, buf, "Total N in Runoff leaving in lbs per ac(S)", 2);
    draw_scale (buf, N_ro_out_max[prev], N_ro_out_min[prev]);
  }
  else if (ANALYSIS && P_SED_ANALYSIS)
  {
    display_map (w_name[0].window_name, window, P_sed_in, "Total P in Sediment generated in lbs per ac(C)", 1);
    draw_scale (P_sed_in, P_sed_in_max[cur], P_sed_in_min[cur]);
    sprintf (buf, "%s_prev", P_sed_in);
    display_map (w_name[1].window_name, window, buf, "Total P in Sediment generated in lbs per ac(S)", 1);
    draw_scale (buf, P_sed_in_max[prev], P_sed_in_min[prev]);
    display_map (w_name[2].window_name, window, P_sed_out, "Total P in Sediment leaving in lbs per ac(C)", 2);
    draw_scale (P_sed_out, P_sed_out_max[cur], P_sed_out_min[cur]);
    sprintf (buf, "%s_prev", P_sed_out);
    display_map (w_name[3].window_name, window, buf, "Total P in Sediment leaving in lbs per ac(S)", 2);
    draw_scale (buf, P_sed_out_max[prev], P_sed_out_min[prev]);
  }
  else if (ANALYSIS && P_RO_ANALYSIS)
  {
    display_map (w_name[0].window_name, window, P_ro_in, "Total P in Runoff generated in lbs per ac(C)", 1);
    draw_scale (P_ro_in, P_ro_in_max[cur], P_ro_in_min[cur]);
    sprintf (buf, "%s_prev", P_ro_in);
    display_map (w_name[1].window_name, window, buf, "Total P in Runoff generated in lbs per ac(S)", 1);
    draw_scale (buf, P_ro_in_max[prev], P_ro_in_min[prev]);
    display_map (w_name[2].window_name, window, P_ro_out, "Total P in Runoff leaving in lbs per ac(C)", 2);
    draw_scale (P_ro_out, P_ro_out_max[cur], P_ro_out_min[cur]);
    sprintf (buf, "%s_prev", P_ro_out);
    display_map (w_name[3].window_name, window, buf, "Total P in Runoff leaving in lbs per ac(S)", 2);
    draw_scale (buf, P_ro_out_max[prev], P_ro_out_min[prev]);
  }
  else if (ANALYSIS && COD_ANALYSIS)
  {
    display_map (w_name[0].window_name, window, COD_ro_in, "Total COD in Runoff generated in lbs per ac(C)", 1);
    draw_scale (COD_ro_in, COD_ro_in_max[cur], COD_ro_in_min[cur]);
    sprintf (buf, "%s_prev", COD_ro_in);
    display_map (w_name[1].window_name, window, buf, "Total COD in Runoff generated in lbs per ac(S)", 1);
    draw_scale (buf, COD_ro_in_max[prev], COD_ro_in_min[prev]);
    display_map (w_name[2].window_name, window, COD_ro_out, "Total COD in Runoff leaving in lbs per ac(C)", 2);
    draw_scale (COD_ro_out, COD_ro_out_max[cur], COD_ro_out_min[cur]);
    sprintf (buf, "%s_prev", COD_ro_out);
    display_map (w_name[3].window_name, window, buf, "Total COD in Runoff leaving in lbs per ac(S)", 2);
    draw_scale (buf, COD_ro_out_max[prev], COD_ro_out_min[prev]);
  }

  R_open_driver ();
  Dchoose (w_name[num_top_row_win].window_name);
  Derase ("black");
  R_close_driver ();

  /*
   * in_wind(outlet_cell[cur],cur);
   */

  histogram (outlet_cell[cur], choice, cur);

  R_open_driver ();
  Dchoose (w_name[num_top_row_win + 1].window_name);
  Derase ("black");
  R_close_driver ();

  histogram (outlet_cell[prev], choice, prev);
  return 0;
}
