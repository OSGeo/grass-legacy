
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

#include "nut_bar.h"
#include "map_gen.h"
#include "window_management.h"

int nut_bar (cell_num, j)
  int cell_num;
  int j;
{
  int draw = YES;
  long int bar_height;		/* height, in pixels, of a histogram bar */
  long int bar_color;		/* color/category number of a histogram
				 * bar */
  long int max_tics;		/* maximum tics allowed on an axis */
  float max = 0.0, min = -99.0;
  long int xoffset;		/* offset for x-axis */
  long int yoffset;		/* offset for y-axis */
  long int stat_start;
  long int stat_finis;
  int text_height;
  int text_width, no_bars;
  long int i;
  long int num_cats = 0;
  long int num_stats = 0;
  long int tic_every;		/* spacing, in units of category value, of
				 * tics */
  long int tic_unit;
  int t, b, l, r;
  int tt, tb, tl, tr;
  int tt1, tb1, tl1, tr1;	/* legend box for labeling */
  int x_line[3];		/* for border of histogram */
  int y_line[3];
  int x_box[5];			/* for histogram bar coordinates */
  int y_box[5];
  double height, width;
  double xscale;		/* scaling factors */
  double yscale;
  char xlabel[1024], legend[512];
  char txt[1024];
  char tic_name[80];
  double bar1, bar2, bar3, bar4;
  int D_get_screen_window (), D_translate_color ();
  int R_set_window (), R_standard_color (), R_get_text_box ();
  int R_move_abs (), R_text (), R_box_abs (), R_polygon_abs ();
  int R_cont_abs (), R_cont_rel (), R_text_size (), R_polyline_abs ();


  /* get coordinates of current screen window, in pixels */
  D_get_screen_window (&t, &b, &l, &r);
  R_set_window (t, b, l, r);

  /* create axis lines, to be drawn later */
  height = b - t;
  width = r - l;
  x_line[0] = x_line[1] = l + (int) (ORIGIN_X * width);
  x_line[2] = l + (int) (XAXIS_END * width);
  y_line[0] = b - (int) (YAXIS_END * height);
  y_line[1] = y_line[2] = b - (int) (ORIGIN_Y * height);

  /* find max and min between the plotting output for the cell */

  max = 0.0;
  min = -99.0;

  if (NUTRIENT == N)
  {
    bar1 = (double) nut_anlys[j][cell_num - 1].N_sed_within;
    bar2 = (double) nut_anlys[j][cell_num - 1].N_sed_outlet;
    bar3 = (double) nut_anlys[j][cell_num - 1].N_sol_within;
    bar4 = (double) nut_anlys[j][cell_num - 1].N_sol_outlet;

    if ((float) bar1 > max)
      max = (float) bar1;
    if ((float) bar2 > max)
      max = (float) bar2;
    if ((float) bar3 > max)
      max = (float) bar3;
    if ((float) bar4 > max)
      max = (float) bar4;

    if ((float) bar1 > min)
      min = (float) bar1;
    if ((float) bar2 < min)
      min = (float) bar2;
    if ((float) bar3 < min)
      min = (float) bar3;
    if ((float) bar4 < min)
      min = (float) bar4;

    no_bars = 4;
  }

  else if (NUTRIENT == P)
  {
    bar1 = (double) nut_anlys[j][cell_num - 1].P_sed_within;
    bar2 = (double) nut_anlys[j][cell_num - 1].P_sed_outlet;
    bar3 = (double) nut_anlys[j][cell_num - 1].P_sol_within;
    bar4 = (double) nut_anlys[j][cell_num - 1].P_sol_outlet;

    if ((float) bar1 > max)
      max = (float) bar1;
    if ((float) bar2 > max)
      max = (float) bar2;
    if ((float) bar3 > max)
      max = (float) bar3;
    if ((float) bar4 > max)
      max = (float) bar4;

    if ((float) bar1 > min)
      min = (float) bar1;
    if ((float) bar2 < min)
      min = (float) bar2;
    if ((float) bar3 < min)
      min = (float) bar3;
    if ((float) bar4 < min)
      min = (float) bar4;

    no_bars = 4;
  }

  else if (NUTRIENT == COD)
  {
    bar1 = (double) nut_anlys[j][cell_num - 1].COD_sol_within;
    bar2 = (double) nut_anlys[j][cell_num - 1].COD_sol_outlet;

    if ((float) bar1 > max)
      max = (float) bar1;
    if ((float) bar2 > max)
      max = (float) bar2;

    if ((float) bar1 > min)
      min = (float) bar1;
    if ((float) bar2 < min)
      min = (float) bar2;

    no_bars = 2;
  }

  /* figure scaling factors and offsets */
  num_cats = 8;
  xscale = ((double) (x_line[2] - x_line[1]) / ((double) num_cats));
  yscale = ((double) (y_line[1] - y_line[0])) / max;
  yoffset = (double) (y_line[1]);

  if (num_cats >= x_line[2] - x_line[1])
    xoffset = (long int) x_line[1];
  else
    xoffset = (long int) x_line[0] + 0.5 * xscale;	/* boxes need extra
							 * space */

  /*
   * figure tic_every and tic_units for the x-axis of the bar-chart.
   * tic_every tells how often to place a tic-number.  tic_unit tells the
   * unit to use in expressing tic-numbers.
   */
  if (xscale < XTIC_DIST)
  {
    max_tics = (x_line[2] - x_line[1]) / XTIC_DIST;
    i = 0;
    while ((num_cats / mtics[i].every) > max_tics)
      i++;
    tic_every = mtics[i].every;
    tic_unit = mtics[i].unit;
    G_strcpy (tic_name, mtics[i].name);
  }
  else
  {
    tic_every = 1;
    tic_unit = 1;
  }

  /*
   * X-AXIS LOOP
   * 
   * loop through category range, drawing a pie-slice and a legend bar on
   * each iteration evenly divisible, a tic-mark on those evenly divisible
   * by tic_unit, and a tic_mark number on those evenly divisible by
   * tic_every
   * 
   */
  R_standard_color (D_translate_color ("white"));
  sprintf (legend, "Legend");
  R_get_text_box (legend, &tt, &tb, &tl, &tr);
  R_move_abs ((int) (r - (tr - tl) - 35), (int) (t + 80 + (b - t) * 0.07));
  R_text (legend);

  for (i = 1; i <= no_bars; i++)
  {
    draw = YES;
    if (i == 1)
    {
      bar_color = D_translate_color ("red");
      R_standard_color (D_translate_color ("red"));
      bar_height = (int) yoffset - yscale * bar1;
      sprintf (legend, "Generated(sed)");
      R_get_text_box (legend, &tt1, &tb1, &tl1, &tr1);
      sprintf (legend, "Generated(sed)");
      R_box_abs ((int) (r - (tr1 - tl1) - 15), 
(int) (t + 90 + (b - t) * 0.07), 
(int) (r - (tr1 - tl1) - 5), 
(int) (t + 100 + (b - t) * 0.07));
      R_move_abs ((int) (r - (tr1 - tl1)), (int) (t + 100 + (b - t) * 0.07));
      R_text (legend);
    }
    if (i == 2)
    {
      bar_color = D_translate_color ("green");
      R_standard_color (D_translate_color ("green"));
      bar_height = (int) yoffset - yscale * bar2;
      sprintf (legend, "Leaving(sed)");
      R_box_abs ((int) (r - (tr1 - tl1) - 15), 
(int) (t + 110 + (b - t) * 0.07), 
(int) (r - (tr1 - tl1) - 5), 
(int) (t + 120 + (b - t) * 0.07));
      R_move_abs ((int) (r - (tr1 - tl1)), (int) (t + 120 + (b - t) * 0.07));
      R_text (legend);
    }
    if (i == 3)
    {
      bar_color = D_translate_color ("yellow");
      R_standard_color (D_translate_color ("yellow"));
      bar_height = (int) yoffset - yscale * bar3;
      sprintf (legend, "Genrated(ro)");
      R_box_abs ((int) (r - (tr1 - tl1) - 15), 
(int) (t + 135 + (b - t) * 0.07), 
(int) (r - (tr1 - tl1) - 5), 
(int) (t + 145 + (b - t) * 0.07));
      R_move_abs ((int) (r - (tr1 - tl1)), (int) (t + 145 + (b - t) * 0.07));
      R_text (legend);
    }
    if (i == 4)
    {
      bar_color = D_translate_color ("grey");
      R_standard_color (D_translate_color ("grey"));
      bar_height = (int) yoffset - yscale * bar4;
      sprintf (legend, "Leaving(ro)");
      R_box_abs ((int) (r - (tr1 - tl1) - 15), 
(int) (t + 155 + (b - t) * 0.07), 
(int) (r - (tr1 - tl1) - 5), 
(int) (t + 165 + (b - t) * 0.07));
      R_move_abs ((int) (r - (tr1 - tl1)), (int) (t + 165 + (b - t) * 0.07));
      R_text (legend);
    }


    /* draw the bar */
    if (draw == YES)
    {
      if (xscale != 1)
      {
	/* draw the bar as a box */
	R_standard_color (bar_color);
	x_box[0] = x_box[1] = xoffset + (i * xscale - 0.5 * xscale);
	x_box[2] = x_box[3] = xoffset + (i * xscale + 0.5 * xscale);
	y_box[0] = y_box[3] = yoffset;
	y_box[1] = y_box[2] = bar_height;
	R_polygon_abs (x_box, y_box, 4);
      }
      else
      {
	/* draw the bar as a line */
	R_standard_color (bar_color);
	x_box[0] = x_box[1] = xoffset + i * xscale;
	y_box[0] = yoffset;
	y_box[1] = bar_height;
	R_move_abs ((int) x_box[0], (int) y_box[0]);
	R_cont_abs ((int) x_box[1], (int) y_box[1]);
      }
    }

    /* draw x-axis tic-marks and numbers */
    if (rem ((long int) i, tic_every) == (float) 0)
    {
      /* draw a numbered tic-mark */
      R_standard_color (D_translate_color ("white"));
      R_move_abs ((int) (xoffset + i * xscale),
		  (int) (b - ORIGIN_Y * (b - t)));
      R_cont_rel ((int) 0, (int) (BIG_TIC * (b - t)));
      sprintf (txt, "%d", (int) (i / tic_unit));
      text_height = (b - t) * TEXT_HEIGHT;
      text_width = (r - l) * TEXT_WIDTH;
      R_text_size (text_width, text_height);
      R_get_text_box (txt, &tt, &tb, &tl, &tr);
      while ((tr - tl) > XTIC_DIST)
      {
	text_width *= 0.75;
	text_height *= 0.75;
	R_text_size (text_width, text_height);
	R_get_text_box (txt, &tt, &tb, &tl, &tr);
      }
      R_move_abs ((int) (xoffset + (i * xscale - (tr - tl) / 2)),
		  (int) (b - XNUMS_Y * (b - t)));
      R_text (txt);
    }
    else if (rem (i, tic_unit) == (float) 0)
    {
      /* draw a tic-mark */
      R_standard_color (D_translate_color ("white"));
      R_move_abs ((int) (xoffset + i * xscale),
		  (int) (b - ORIGIN_Y * (b - t)));
      R_cont_rel ((int) 0, (int) (SMALL_TIC * (b - t)));
    }
  }

  /* draw the x-axis label */
  if (tic_unit != 1)
    sprintf (xlabel, "X-AXIS: Nutrient Values ");
  else
    sprintf (xlabel, "X-AXIS: Nutrient Values");
  text_height = (b - t) * TEXT_HEIGHT;
  text_width = (r - l) * TEXT_WIDTH * 2.0;
  R_text_size (text_width, text_height);
  R_get_text_box (xlabel, &tt, &tb, &tl, &tr);
  R_move_abs ((int) (l + (r - l) / 2 - (tr - tl) / 2), 
(int) (b - LABEL_1 * (b - t)));
  R_standard_color ((int) D_translate_color ("white"));
  R_text (xlabel);

  /*
   * DRAW Y-AXIS TIC-MARKS AND NUMBERS
   * 
   * first, figure tic_every and tic_units for the x-axis of the bar-chart.
   * tic_every tells how often to place a tic-number.  tic_unit tells the
   * unit to use in expressing tic-numbers.
   */

  max_tics = (long) ((y_line[1] - y_line[0]) / YTIC_DIST);

  num_stats = (int) (max - min) + 1;
  i = 0;
  while ((num_stats / mtics[i].every) > max_tics)
    i++;
  tic_every = mtics[i].every;
  tic_unit = mtics[i].unit;
  G_strcpy (tic_name, mtics[i].name);

  stat_start = tic_unit * ((long) (min / (float) tic_unit));
  stat_finis = tic_unit * ((long) (max / (float) tic_unit));


  /*
   * Y-AXIS LOOP
   * 
   */
  for (i = stat_start; i <= stat_finis; i += tic_unit)
  {
    if (rem (i, tic_every) == (float) 0)
    {
      /* draw a tic-mark */
      R_move_abs ((int) x_line[0], (int) (yoffset - yscale * i));
      R_cont_rel ((int) (-(r - l) * BIG_TIC), (int) 0);

      /* draw a tic-mark number */
      sprintf (txt, "%d", (int) (i / tic_unit));
      text_height = (b - t) * TEXT_HEIGHT;
      text_width = (r - l) * TEXT_WIDTH;
      R_text_size (text_width, text_height);
      R_get_text_box (txt, &tt, &tb, &tl, &tr);
      while ((tt - tb) > YTIC_DIST)
      {
	text_width *= 0.75;
	text_height *= 0.75;
	R_text_size (text_width, text_height);
	R_get_text_box (txt, &tt, &tb, &tl, &tr);
      }
      R_move_abs ((int) (l + (r - l) * YNUMS_X - (tr - tl) / 2),
		  (int) (yoffset - (yscale * i + 0.5 * (tt - tb))));
      R_text (txt);
    }
    else if (rem (i, tic_unit) == (float) 0)
    {
      /* draw a tic-mark */
      R_move_abs ((int) x_line[0], (int) (yoffset - yscale * i));
      R_cont_rel ((int) (-(r - l) * SMALL_TIC), (int) 0);
    }
  }

  /* draw the y-axis label */
  if (tic_unit != 1)
    sprintf (xlabel, "Y-AXIS: %s of lbs/ac", tic_name);
  else
    sprintf (xlabel, "Y-AXIS: in lbs/ac");

  text_height = (b - t) * TEXT_HEIGHT;
  text_width = (r - l) * TEXT_WIDTH * 2.0;
  R_text_size (text_width, text_height);
  R_get_text_box (xlabel, &tt, &tb, &tl, &tr);
  R_move_abs ((int) (l + (r - l) / 2 - (tr - tl) / 2), 
(int) (b - LABEL_2 * (b - t)));
  R_standard_color ((int) D_translate_color ("white"));
  R_text (xlabel);

  /* draw x and y axis lines */
  R_standard_color (D_translate_color ("white"));
  R_polyline_abs (x_line, y_line, 3);
  return 0;
}

/*
 * float rem(x,y) long int x,y; { long int d = x/y; return((float)(x-y*d)); } */
