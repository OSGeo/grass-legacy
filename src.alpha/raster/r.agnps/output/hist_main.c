
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
 * main.c
 * 
 * function defined:
 * 
 * main for program Dhistogram
 * 
 * 
 * PURPOSE: To draw a bar-chart or a pie-chart representing the histogram
 * statistics of a cell-file
 * 
 * Usage:
 * 
 * Dhistogram mapname [color] Dhistogram name=mapname [color=color]
 * [type=type] [style=style]
 * 
 * The color option specifies the color for the labels, tic-marks, and
 * borders of the chart.  Use one of the 16 standard GRASS color names.
 * The type option is either "area" or "cells," the default is "cells" The
 * style option is either "pie" or "bar," the default is "bar"
 * 
 * Dave Johnson DBA Systems, Inc. 10560 Arrowhead Drive Fairfax, Virginia
 * 22030
 * 
 */

#include "map_gen.h"
#include "window_management.h"

int histogram (cell_num, choice, j)
  int cell_num;
  int choice;
  int j;
{
  int text_height;
  int text_width;
  char window_name[64];
  char title[512];
  int tt, tb, tl, tr;
  int t, b, l, r;
  int bar(), nut_bar();

  int R_open_driver (), D_get_cur_wind (), G_fatal_error (), D_set_cur_wind ();
  int R_font (), D_get_screen_window (), R_text_size (), R_get_text_box ();
  int R_move_abs (), R_standard_color (), D_translate_color (), R_text ();
  int R_flush(), R_close_driver();

  /* set up the graphics driver and initialize its color-table */

  R_open_driver ();

  if (D_get_cur_wind (window_name))
    G_fatal_error ("No current window");

  if (D_set_cur_wind (window_name))
    G_fatal_error ("Current window not available");

  /* draw a title for */

  if (cell_num < no_cells[j] + 2)
    sprintf (title, "Output for the Cell Number %d", cell_num);
  else
    sprintf (title, "Average Output for the selected area ");

  R_font ("romans");
  D_get_screen_window (&t, &b, &l, &r);
  text_height = (b - t) * 0.04;
  text_width = (r - l) * 0.05 * 0.7;
  R_text_size (text_width, text_height);
  R_get_text_box (title, &tt, &tb, &tl, &tr);
  R_move_abs ((int) (l + (r - l) / 2 - (tr - tl) / 2),
	      (int) (t - 10 + (b - t) * 0.07));
  R_standard_color (D_translate_color ("white"));
  R_text (title);

  text_width = (r - l) * 0.05 * 0.6;
  R_text_size (text_width, text_height);

  if (cell_num < no_cells[j] + 2)
    sprintf (title, "Drainage Area to the Cell is %d acre(s)",
	     cell_runoff[j][cell_num - 1].dr_area);
  else
    sprintf (title, " ");
  R_get_text_box (title, &tt, &tb, &tl, &tr);
  R_move_abs ((int) (l + (r - l) / 2 - (tr - tl) / 2),
	      (int) (t + 10 + (b - t) * 0.07));
  R_text (title);

  if (choice == soil_loss)
  {
    text_width = (r - l) * 0.05 * 0.7;
    R_text_size (text_width, text_height);
    sprintf (title, "Detachment from Cell is %5.2f tons/acre", cell_sediment[j][cell_num - 1].cell_erosion[5]);
    R_get_text_box (title, &tt, &tb, &tl, &tr);
    R_move_abs ((int) (l + (r - l) / 2 - (tr - tl) / 2),
		(int) (t + 30 + (b - t) * 0.07));
    R_text (title);


    sprintf (title, "Cell Deposition is %d percent", cell_sediment[j][cell_num - 1].deposition[5]);
    R_get_text_box (title, &tt, &tb, &tl, &tr);
    R_move_abs ((int) (l + (r - l) / 2 - (tr - tl) / 2),
		(int) (t + 50 + (b - t) * 0.07));
    R_text (title);

    /* plot the distributrion statistics */

    bar (cell_num, choice, j);
  }

  else if (choice == runoff)
  {
    text_width = (r - l) * 0.05 * 0.7;
    R_text_size (text_width, text_height);
    sprintf (title, "Peak flow from Upstream Cell is %d cfs", cell_runoff[j][cell_num - 1].peak_us);
    R_get_text_box (title, &tt, &tb, &tl, &tr);
    R_move_abs ((int) (l + (r - l) / 2 - (tr - tl) / 2),
		(int) (t + 30 + (b - t) * 0.07));
    R_text (title);


    sprintf (title, "Peak flow to Downstream Cell is %d cfs", cell_runoff[j][cell_num - 1].peak_ds);
    R_get_text_box (title, &tt, &tb, &tl, &tr);
    R_move_abs ((int) (l + (r - l) / 2 - (tr - tl) / 2),
		(int) (t + 50 + (b - t) * 0.07));
    R_text (title);

    /* plot the distributrion statistics */

    bar (cell_num, choice, j);
  }

  else if (choice == nutrients)
  {

    text_width = (r - l) * 0.05 * 0.6;
    R_text_size (text_width, text_height);

    if (NUTRIENT == N)
      sprintf (title, "N water soulble concentration is %d ppm",
	       nut_anlys[j][cell_num - 1].N_conc);
    else if (NUTRIENT == P)
      sprintf (title, "P water soulble concentration is %d ppm",
	       nut_anlys[j][cell_num - 1].P_conc);
    else if (NUTRIENT == COD)
      sprintf (title, "COD water soulble concentration is %d ppm",
	       nut_anlys[j][cell_num - 1].COD_conc);


    R_get_text_box (title, &tt, &tb, &tl, &tr);
    R_move_abs ((int) (l + (r - l) / 2 - (tr - tl) / 2),
		(int) (t + 30 + (b - t) * 0.07));
    R_text (title);

    /* plot the distributrion statistics */

    nut_bar (cell_num, j);
  }


  R_flush ();
  R_close_driver ();
  return 0;
}
