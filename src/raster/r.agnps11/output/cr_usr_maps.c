
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


int cr_usr_maps (max, min, choice, j)
  float max, min;
  int choice, j;
{

  int i;
  float temp1, temp2, temp3;
  char buf[512];
  char *t1, *t2, *t3;
  FILE *fs, *ft, *fu;
  struct Colors sed_in_colors, sed_gen_colors, sed_out_colors;
  void working_sngl ();
  int make_grn_yel_red ();
  int G_init_colors (), G_set_color (), G_system ();
  int G_write_colors (), G_free_colors ();

  working_sngl ();
  printf ("Creating User specified range maps\n");

  /* initiate the colors structures for the maps */
  G_init_colors (&sed_in_colors);
  G_init_colors (&sed_gen_colors);
  G_init_colors (&sed_out_colors);

  t1 = G_tempfile ();
  t2 = G_tempfile ();
  t3 = G_tempfile ();

  fs = fopen (t1, "w");
  ft = fopen (t2, "w");
  fu = fopen (t3, "w");

  for (i = 0; i < no_cells[cur]; i++)
  {

    /* create user choice maps depending on which screen they are */
    if (choice == soil_loss)
    {
      if (min <= cell_sediment[j][i].gen_above[5] && max >= cell_sediment[j][i].gen_above[5])
	temp1 = cell_sediment[j][i].gen_above[5];
      else
	temp1 = 0.0;
      if (min <= cell_sediment[j][i].within[5] && max >= cell_sediment[j][i].within[5])
	temp2 = cell_sediment[j][i].within[5];
      else
	temp2 = 0.0;
      if (min <= cell_sediment[j][i].yield[5] && max >= cell_sediment[j][i].yield[5])
	temp3 = cell_sediment[j][i].yield[5];
      else
	temp3 = 0.0;
    }

    else if (choice == runoff)
    {
      if (min <= cell_runoff[j][i].us_ro && max >= cell_runoff[j][i].us_ro)
	temp1 = cell_runoff[j][i].us_ro;
      else
	temp1 = 0.0;
      if (min <= cell_runoff[j][i].overland_ro && max >= cell_runoff[j][i].overland_ro)
	temp2 = cell_runoff[j][i].overland_ro;
      else
	temp2 = 0.0;
      if (min <= cell_runoff[j][i].ds_ro && max >= cell_runoff[j][i].ds_ro)
	temp3 = cell_runoff[j][i].ds_ro;
      else
	temp3 = 0.0;
    }

    else if (choice == nutrients)
    {
      if (NUTRIENT == N && NUT_ATTCH == sed)
      {
	if (min <= nut_anlys[j][i].N_sed_within && max >= nut_anlys[j][i].N_sed_within)
	  temp1 = nut_anlys[j][i].N_sed_within;
	else
	  temp1 = 0.0;
	if (min <= nut_anlys[j][i].N_sed_outlet && max >= nut_anlys[j][i].N_sed_outlet)
	  temp2 = nut_anlys[j][i].N_sed_outlet;
	else
	  temp2 = 0.0;
      }
      else if (NUTRIENT == N && NUT_ATTCH == ro)
      {
	if (min <= nut_anlys[j][i].N_sol_within && max >= nut_anlys[j][i].N_sol_within)
	  temp1 = nut_anlys[j][i].N_sol_within;
	else
	  temp1 = 0.0;
	if (min <= nut_anlys[j][i].N_sol_outlet && max >= nut_anlys[j][i].N_sol_outlet)
	  temp2 = nut_anlys[j][i].N_sol_outlet;
	else
	  temp2 = 0.0;
      }
      else if (NUTRIENT == P && NUT_ATTCH == sed)
      {
	if (min <= nut_anlys[j][i].P_sed_within && max >= nut_anlys[j][i].P_sed_within)
	  temp1 = nut_anlys[j][i].P_sed_within;
	else
	  temp1 = 0.0;
	if (min <= nut_anlys[j][i].P_sed_outlet && max >= nut_anlys[j][i].P_sed_outlet)
	  temp2 = nut_anlys[j][i].P_sed_outlet;
	else
	  temp2 = 0.0;
      }
      else if (NUTRIENT == P && NUT_ATTCH == ro)
      {
	if (min <= nut_anlys[j][i].P_sol_within && max >= nut_anlys[j][i].P_sol_within)
	  temp1 = nut_anlys[j][i].P_sol_within;
	else
	  temp1 = 0.0;
	if (min <= nut_anlys[j][i].P_sol_outlet && max >= nut_anlys[j][i].P_sol_outlet)
	  temp2 = nut_anlys[j][i].P_sol_outlet;
	else
	  temp2 = 0.0;
      }
      else if (NUTRIENT == COD)
      {
	if (min <= nut_anlys[j][i].COD_sol_within && max >= nut_anlys[j][i].COD_sol_within)
	  temp1 = nut_anlys[j][i].COD_sol_within;
	else
	  temp1 = 0.0;
	if (min <= nut_anlys[j][i].COD_sol_outlet && max >= nut_anlys[j][i].COD_sol_outlet)
	  temp2 = nut_anlys[j][i].COD_sol_outlet;
	else
	  temp2 = 0.0;
      }
    }

    fprintf (fs, "%d = %d \n", i + 1, (int) (temp1 * sig_fac));
    fprintf (ft, "%d = %d \n", i + 1, (int) (temp2 * sig_fac));
    fprintf (fu, "%d = %d \n", i + 1, (int) (temp3 * sig_fac));

  }

  fclose (fs);
  fclose (ft);
  fclose (fu);

  make_grn_yel_red (&sed_in_colors, (int) (min * sig_fac), (int) (max * sig_fac));

  make_grn_yel_red (&sed_gen_colors, (int) (min * sig_fac), (int) (max * sig_fac));

  make_grn_yel_red (&sed_out_colors, (int) (min * sig_fac), (int) (max * sig_fac));


  G_set_color ((CELL) 0, 0, 0, 0, &sed_out_colors);
  G_set_color ((CELL) 0, 0, 0, 0, &sed_gen_colors);
  G_set_color ((CELL) 0, 0, 0, 0, &sed_in_colors);

  if (choice == soil_loss)
  {
    if (j == cur)
    {
      sprintf (buf, "r.reclass input=%s output=%s < %s", cell_num_map->p, sed_above, t1);
      G_system (buf);

      sprintf (buf, "r.reclass input=%s output=%s < %s", cell_num_map->p, sed_gented, t2);
      G_system (buf);

      sprintf (buf, "r.reclass input=%s output=%s < %s", cell_num_map->p, sed_yield, t3);
      G_system (buf);

      G_write_colors (sed_above, this_mapset, &sed_in_colors);
      G_write_colors (sed_gented, this_mapset, &sed_gen_colors);
      G_write_colors (sed_yield, this_mapset, &sed_out_colors);
    }

    if (j == prev)
    {
      sprintf (buf, "r.reclass input=%s output=%s_prev < %s", cell_num_map->p, sed_above, t1);
      G_system (buf);

      sprintf (buf, "r.reclass input=%s output=%s_prev < %s", cell_num_map->p, sed_gented, t2);
      G_system (buf);

      sprintf (buf, "r.reclass input=%s output=%s_prev < %s", cell_num_map->p, sed_yield, t3);
      G_system (buf);

      sprintf (buf, "%s_prev", sed_above);
      G_write_colors (buf, this_mapset, &sed_in_colors);
      sprintf (buf, "%s_prev", sed_gented);
      G_write_colors (buf, this_mapset, &sed_gen_colors);
      sprintf (buf, "%s_prev", sed_yield);
      G_write_colors (buf, this_mapset, &sed_out_colors);
    }

  }

  else if (choice == runoff)
  {
    if (j == cur)
    {
      sprintf (buf, "r.reclass input=%s output=%s < %s", cell_num_map->p, usr_ro_us, t1);
      G_system (buf);

      sprintf (buf, "r.reclass input=%s output=%s < %s", cell_num_map->p, usr_ro_gen, t2);
      G_system (buf);

      sprintf (buf, "r.reclass input=%s output=%s < %s", cell_num_map->p, usr_ro_ds, t3);
      G_system (buf);

      G_write_colors (usr_ro_us, this_mapset, &sed_in_colors);
      G_write_colors (usr_ro_gen, this_mapset, &sed_gen_colors);
      G_write_colors (usr_ro_ds, this_mapset, &sed_out_colors);
    }
    if (j == prev)
    {
      sprintf (buf, "r.reclass input=%s output=%s_prev < %s", cell_num_map->p, usr_ro_us, t1);
      G_system (buf);

      sprintf (buf, "r.reclass input=%s output=%s_prev < %s", cell_num_map->p, usr_ro_gen, t2);
      G_system (buf);

      sprintf (buf, "r.reclass input=%s output=%s_prev < %s", cell_num_map->p, usr_ro_ds, t3);
      G_system (buf);

      sprintf (buf, "%s_prev", usr_ro_us);
      G_write_colors (buf, this_mapset, &sed_in_colors);
      sprintf (buf, "%s_prev", usr_ro_gen);
      G_write_colors (buf, this_mapset, &sed_gen_colors);
      sprintf (buf, "%s_prev", usr_ro_ds);
      G_write_colors (buf, this_mapset, &sed_out_colors);
    }
  }

  else if (choice == nutrients && j == cur)
  {
    if (NUTRIENT == N && NUT_ATTCH == sed)
    {
      sprintf (buf, "r.reclass input=%s output=%s < %s", cell_num_map->p, usr_N_sed_in, t1);
      G_system (buf);

      sprintf (buf, "r.reclass input=%s output=%s < %s", cell_num_map->p, usr_N_sed_out, t2);
      G_system (buf);

      G_write_colors (usr_N_sed_in, this_mapset, &sed_in_colors);
      G_write_colors (usr_N_sed_out, this_mapset, &sed_gen_colors);
    }

    else if (NUTRIENT == N && NUT_ATTCH == ro)
    {
      sprintf (buf, "r.reclass input=%s output=%s < %s", cell_num_map->p, usr_N_ro_in, t1);
      G_system (buf);

      sprintf (buf, "r.reclass input=%s output=%s < %s", cell_num_map->p, usr_N_ro_out, t2);
      G_system (buf);

      G_write_colors (usr_N_ro_in, this_mapset, &sed_in_colors);
      G_write_colors (usr_N_ro_out, this_mapset, &sed_gen_colors);

    }
    else if (NUTRIENT == P && NUT_ATTCH == sed)
    {
      sprintf (buf, "r.reclass input=%s output=%s < %s", cell_num_map->p, usr_P_sed_in, t1);
      G_system (buf);

      sprintf (buf, "r.reclass input=%s output=%s < %s", cell_num_map->p, usr_P_sed_out, t2);
      G_system (buf);

      G_write_colors (usr_P_sed_in, this_mapset, &sed_in_colors);
      G_write_colors (usr_P_sed_out, this_mapset, &sed_gen_colors);

    }

    else if (NUTRIENT == P && NUT_ATTCH == ro)
    {
      sprintf (buf, "r.reclass input=%s output=%s < %s", cell_num_map->p, usr_P_ro_in, t1);
      G_system (buf);

      sprintf (buf, "r.reclass input=%s output=%s < %s", cell_num_map->p, usr_P_ro_out, t2);
      G_system (buf);

      G_write_colors (usr_P_ro_in, this_mapset, &sed_in_colors);
      G_write_colors (usr_P_ro_out, this_mapset, &sed_gen_colors);

    }
    else if (NUTRIENT == COD)
    {
      sprintf (buf, "r.reclass input=%s output=%s < %s", cell_num_map->p, usr_COD_ro_in, t1);
      G_system (buf);

      sprintf (buf, "r.reclass input=%s output=%s < %s", cell_num_map->p, usr_COD_ro_out, t2);
      G_system (buf);

      G_write_colors (usr_COD_ro_in, this_mapset, &sed_in_colors);
      G_write_colors (usr_COD_ro_out, this_mapset, &sed_gen_colors);
    }
  }

  else if (choice == nutrients && j == prev)
  {
    if (NUTRIENT == N && NUT_ATTCH == sed)
    {
      sprintf (buf, "r.reclass input=%s output=%s_prev < %s", cell_num_map->p, usr_N_sed_in, t1);
      G_system (buf);

      sprintf (buf, "r.reclass input=%s output=%s_prev < %s", cell_num_map->p, usr_N_sed_out, t2);
      G_system (buf);

      sprintf (buf, "%s_prev", usr_N_sed_in);
      G_write_colors (buf, this_mapset, &sed_in_colors);
      sprintf (buf, "%s_prev", usr_N_sed_out);
      G_write_colors (buf, this_mapset, &sed_gen_colors);
    }

    else if (NUTRIENT == N && NUT_ATTCH == ro)
    {
      sprintf (buf, "r.reclass input=%s output=%s_prev < %s", cell_num_map->p, usr_N_ro_in, t1);
      G_system (buf);

      sprintf (buf, "r.reclass input=%s output=%s_prev < %s", cell_num_map->p, usr_N_ro_out, t2);
      G_system (buf);

      sprintf (buf, "%s_prev", usr_N_ro_in);
      G_write_colors (buf, this_mapset, &sed_in_colors);
      sprintf (buf, "%s_prev", usr_N_ro_out);
      G_write_colors (buf, this_mapset, &sed_gen_colors);

    }
    else if (NUTRIENT == P && NUT_ATTCH == sed)
    {
      sprintf (buf, "r.reclass input=%s output=%s_prev < %s", cell_num_map->p, usr_P_sed_in, t1);
      G_system (buf);

      sprintf (buf, "r.reclass input=%s output=%s_prev < %s", cell_num_map->p, usr_P_sed_out, t2);
      G_system (buf);

      sprintf (buf, "%s_prev", usr_P_sed_in);
      G_write_colors (buf, this_mapset, &sed_in_colors);
      sprintf (buf, "%s_prev", usr_P_sed_out);
      G_write_colors (buf, this_mapset, &sed_gen_colors);

    }

    else if (NUTRIENT == P && NUT_ATTCH == ro)
    {
      sprintf (buf, "r.reclass input=%s output=%s_prev < %s", cell_num_map->p, usr_P_ro_in, t1);
      G_system (buf);

      sprintf (buf, "r.reclass input=%s output=%s_prev < %s", cell_num_map->p, usr_P_ro_out, t2);
      G_system (buf);

      sprintf (buf, "%s_prev", usr_P_ro_in);
      G_write_colors (buf, this_mapset, &sed_in_colors);
      sprintf (buf, "%s_prev", usr_P_ro_out);
      G_write_colors (buf, this_mapset, &sed_gen_colors);

    }
    else if (NUTRIENT == COD)
    {
      sprintf (buf, "r.reclass input=%s output=%s_prev < %s", cell_num_map->p, usr_COD_ro_in, t1);
      G_system (buf);

      sprintf (buf, "r.reclass input=%s output=%s_prev < %s", cell_num_map->p, usr_COD_ro_out, t2);
      G_system (buf);

      sprintf (buf, "%s_prev", usr_COD_ro_in);
      G_write_colors (buf, this_mapset, &sed_in_colors);
      sprintf (buf, "%s_prev", usr_COD_ro_out);
      G_write_colors (buf, this_mapset, &sed_gen_colors);
    }
  }

  G_free_colors (&sed_in_colors);
  G_free_colors (&sed_gen_colors);
  G_free_colors (&sed_out_colors);
  return 0;
}

int dis_usr_cr_maps (max, min, choice)
  float max, min;
  int choice;
{
  struct Cell_head window;
  char buf[64];
  int G__get_window ();
  int display_map ();
  int draw_scale ();


  G__get_window (&window, "", "WIND", this_mapset);

  if (choice == soil_loss)
  {
    display_map (w_name[0].window_name, window, sed_above,
		 "Erosion in tons", 1);
    draw_scale (sed_above, max, min);
    display_map (w_name[1].window_name, window, sed_gented,
		 "Deposition in tons", 2);
    draw_scale (sed_gented, max, min);
    display_map (w_name[2].window_name, window, sed_yield,
		 "Sediment leaving cell in tons", 3);
    draw_scale (sed_yield, max, min);
  }

  else if (choice == runoff)
  {
    display_map (w_name[0].window_name, window, usr_ro_us,
		 "Runoff from Upstream in inches", 1);
    draw_scale (usr_ro_us, max, min);
    display_map (w_name[1].window_name, window, usr_ro_gen,
		 "Runoff generated in inches", 2);
    draw_scale (usr_ro_gen, max, min);
    display_map (w_name[2].window_name, window, usr_ro_ds,
		 "Runoff to Downstream in inches", 3);
    draw_scale (usr_ro_ds, max, min);
  }

  else if (choice == nutrients)
  {
    if (NUTRIENT == N && NUT_ATTCH == sed)
    {
      display_map (w_name[0].window_name, window, usr_N_sed_in,
		   "Total N in Sediment generated in lbs per ac", 1);
      draw_scale (usr_N_sed_in, max, min);
      display_map (w_name[1].window_name, window, usr_N_sed_out,
		   "Total N in Sediment leaving in lbs per ac", 2);
      draw_scale (usr_N_sed_out, max, min);
    }

    else if (NUTRIENT == N && NUT_ATTCH == ro)
    {
      display_map (w_name[0].window_name, window, usr_N_ro_in,
		   "Total N in Runoff generated in lbs per ac", 1);
      draw_scale (usr_N_ro_in, max, min);
      display_map (w_name[1].window_name, window, usr_N_ro_out,
		   "Total N in Runoff leaving in lbs per ac", 2);
      draw_scale (usr_N_ro_out, max, min);
    }

    else if (NUTRIENT == P && NUT_ATTCH == sed)
    {
      display_map (w_name[0].window_name, window, usr_P_sed_in,
		   "Total P in Sediment generated in lbs per ac", 1);
      draw_scale (usr_P_sed_in, max, min);
      display_map (w_name[1].window_name, window, usr_P_sed_out,
		   "Total P in Sediment leaving in lbs per ac", 2);
      draw_scale (usr_P_sed_out, max, min);
    }

    else if (NUTRIENT == P && NUT_ATTCH == ro)
    {
      display_map (w_name[0].window_name, window, usr_P_ro_in,
		   "Total P in Runoff generated in lbs per ac", 1);
      draw_scale (usr_P_ro_in, max, min);
      display_map (w_name[1].window_name, window, usr_P_ro_out,
		   "Total P in Runoff leaving in lbs per ac", 2);
      draw_scale (usr_P_ro_out, max, min);
    }

    else if (NUTRIENT == COD)
    {
      display_map (w_name[0].window_name, window, usr_COD_ro_in,
		   "Total COD in Runoff generated in lbs per ac", 1);
      draw_scale (usr_COD_ro_in, max, min);
      display_map (w_name[1].window_name, window, usr_COD_ro_out,
		   "Total COD in Runoff leaving in lbs per ac", 2);
      draw_scale (usr_COD_ro_out, max, min);
    }
  }

  else if (choice == analysis)
  {
    if (SED_ANALYSIS)
    {
      display_map (w_name[0].window_name, window, sed_above,
		   "Erosion in tons(C)", 1);
      draw_scale (sed_above, max, min);
      sprintf (buf, "%s_prev", sed_above);
      display_map (w_name[1].window_name, window, buf,
		   "Erosion in tons(S)", 1);
      draw_scale (buf, max, min);
      display_map (w_name[2].window_name, window, sed_gented,
		   "Deposition in tons(C)", 2);
      draw_scale (sed_gented, max, min);
      sprintf (buf, "%s_prev", sed_gented);
      display_map (w_name[3].window_name, window, buf,
		   "Deposition in tons(S)", 2);
      draw_scale (buf, max, min);
    }
    else if (RO_ANALYSIS)
    {
      display_map (w_name[0].window_name, window, usr_ro_gen,
		   "Runoff generated in inches(C)", 2);
      draw_scale (usr_ro_gen, max, min);
      sprintf (buf, "%s_prev", usr_ro_gen);
      display_map (w_name[1].window_name, window, buf,
		   "Runoff generated in inches(S)", 2);
      draw_scale (buf, max, min);
      display_map (w_name[2].window_name, window, usr_ro_ds,
		   "Runoff to Downstream in inches(C)", 3);
      draw_scale (usr_ro_ds, max, min);
      sprintf (buf, "%s_prev", usr_ro_ds);
      display_map (w_name[3].window_name, window, buf,
		   "Runoff to Downstream in inches(S)", 3);
      draw_scale (buf, max, min);
    }
    else if (N_SED_ANALYSIS)
    {
      display_map (w_name[0].window_name, window, usr_N_sed_in,
		   "Total N in Sediment generated in lbs per ac(C)", 1);
      draw_scale (usr_N_sed_in, max, min);
      sprintf (buf, "%s_prev", usr_N_sed_in);
      display_map (w_name[1].window_name, window, buf,
		   "Total N in Sediment generated in lbs per ac(S)", 1);
      draw_scale (buf, max, min);
      display_map (w_name[2].window_name, window, usr_N_sed_out,
		   "Total N in Sediment leaving in lbs per ac(C)", 2);
      draw_scale (usr_N_sed_out, max, min);
      sprintf (buf, "%s_prev", usr_N_sed_out);
      display_map (w_name[3].window_name, window, buf,
		   "Total N in Sediment leaving in lbs per ac(S)", 2);
      draw_scale (buf, max, min);
    }
    else if (N_RO_ANALYSIS)
    {
      display_map (w_name[0].window_name, window, usr_N_ro_in,
		   "Total N in Runoff generated in lbs per ac(C)", 1);
      draw_scale (usr_N_ro_in, max, min);
      sprintf (buf, "%s_prev", usr_N_ro_in);
      display_map (w_name[1].window_name, window, buf,
		   "Total N in Runoff generated in lbs per ac(S)", 1);
      draw_scale (buf, max, min);
      display_map (w_name[2].window_name, window, usr_N_ro_out,
		   "Total N in Runoff leaving in lbs per ac(C)", 2);
      draw_scale (usr_N_ro_out, max, min);
      sprintf (buf, "%s_prev", usr_N_ro_out);
      display_map (w_name[3].window_name, window, buf,
		   "Total N in Runoff leaving in lbs per ac(S)", 2);
      draw_scale (buf, max, min);
    }
    else if (P_RO_ANALYSIS)
    {
      display_map (w_name[0].window_name, window, usr_P_ro_in,
		   "Total P in Runoff generated in lbs per ac(C)", 1);
      draw_scale (usr_P_ro_in, max, min);
      sprintf (buf, "%s_prev", usr_P_ro_in);
      display_map (w_name[2].window_name, window, buf,
		   "Total P in Runoff generated in lbs per ac(S)", 1);
      draw_scale (buf, max, min);
      display_map (w_name[2].window_name, window, usr_P_ro_out,
		   "Total P in Runoff leaving in lbs per ac(C)", 2);
      draw_scale (usr_P_ro_out, max, min);
      sprintf (buf, "%s_prev", usr_P_ro_out);
      display_map (w_name[3].window_name, window, buf,
		   "Total P in Runoff leaving in lbs per ac(S)", 2);
      draw_scale (buf, max, min);
    }
    else if (P_SED_ANALYSIS)
    {
      display_map (w_name[0].window_name, window, usr_P_sed_in,
		   "Total P in Sediment generated in lbs per ac(C)", 1);
      draw_scale (usr_P_sed_in, max, min);
      sprintf (buf, "%s_prev", usr_P_sed_in);
      display_map (w_name[1].window_name, window, buf,
		   "Total P in Sediment generated in lbs per ac(S)", 1);
      draw_scale (buf, max, min);
      display_map (w_name[2].window_name, window, usr_P_sed_out,
		   "Total P in Sediment leaving in lbs per ac(C)", 2);
      draw_scale (usr_P_sed_out, max, min);
      sprintf (buf, "%s_prev", usr_P_sed_out);
      display_map (w_name[3].window_name, window, buf,
		   "Total P in Sediment leaving in lbs per ac(S)", 2);
      draw_scale (buf, max, min);
    }
    else if (COD_ANALYSIS)
    {
      display_map (w_name[0].window_name, window, usr_COD_ro_in,
		   "Total COD in Runoff generated in lbs per ac(C)", 1);
      draw_scale (usr_COD_ro_in, max, min);
      sprintf (buf, "%s_prev", usr_COD_ro_in);
      display_map (w_name[1].window_name, window, buf,
		   "Total COD in Runoff generated in lbs per ac(S)", 1);
      draw_scale (buf, max, min);
      display_map (w_name[2].window_name, window, usr_COD_ro_out,
		   "Total COD in Runoff leaving in lbs per ac(C)", 2);
      draw_scale (usr_COD_ro_out, max, min);
      sprintf (buf, "%s_prev", usr_COD_ro_out);
      display_map (w_name[3].window_name, window, buf,
		   "Total COD in Runoff leaving in lbs per ac(S)", 2);
      draw_scale (buf, max, min);
    }

  }
  return 0;
}
