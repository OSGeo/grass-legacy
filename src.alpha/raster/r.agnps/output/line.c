
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

#include "line.h"
#include "map_gen.h"
#include "window_management.h"

#define MAX(x,y)             (( x>y ) ? x : y)
#define MIN(x,y)             (( x<y ) ? x : y)

int line (xlabel1, flag)
  char xlabel1[1024];
  int flag;
{
  int draw = YES;
  long int bar_height;		/* height, in pixels, of a histogram bar */
  long int max_tics;		/* maximum tics allowed on an axis */
  float max = 0.0, min = 99999.0;
  long int xoffset;		/* offset for x-axis */
  long int yoffset;		/* offset for y-axis */
  long int stat_start;
  long int stat_finis;
  int text_height;
  int text_width;
  long int i;
  long int num_cats = 0;
  long int num_stats = 0;
  long int tic_every;		/* spacing, in units of category value, of
				 * tics */
  long int tic_unit;
  int t, b, l, r;
  int tt, tb, tl, tr;
  int tt1, tb1, tl1, tr1;	/* legend box for labeling */
  int prev_x, prev_y;
  int x_line[3];		/* for border of histogram */
  int y_line[3];
  int x_box[5];			/* for histogram bar coordinates */
  int y_box[5];
  double height, width;
  double xscale;		/* scaling factors */
  double yscale;
  double fac;
  char txt[1024], xlabel[1024];
  int D_get_screen_window (), R_set_window (), R_text_size ();
  int R_standard_color (), D_translate_color (), R_move_abs ();
  int R_cont_abs (), R_get_text_box (), R_text (), R_cont_rel ();
  int R_polyline_abs (), R_flush ();

  printf ("Entering line program\n");

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

  text_height = (b - t) * TEXT_HEIGHT;
  text_width = (r - l) * TEXT_WIDTH;
  R_text_size (text_width, text_height);

  if (flag == YES)
  {
    R_standard_color (D_translate_color ("red"));
    R_move_abs ((int) (x_line[2] - 0.1 * width), (int) (y_line[0] - 0.050 * height));
    R_cont_abs ((int) (x_line[2]), (int) (y_line[0] - 0.05 * height));
    R_move_abs ((int) (x_line[2] + 0.05 * width), (int) (y_line[0] - 0.05 * height));
    sprintf (xlabel, "Cumulative");
    R_get_text_box (xlabel, &tt1, &tb1, &tl1, &tr1);
    R_standard_color (D_translate_color ("white"));
    R_text (xlabel);
  }
  else
  {
    R_standard_color (D_translate_color ("green"));
    R_move_abs ((int) (x_line[2] - 0.1 * width), (int) (y_line[0] + 0.025 * height));
    R_cont_abs ((int) (x_line[2]), (int) (y_line[0] + 0.025 * height));
    R_move_abs ((int) (x_line[2] + 0.05 * width), (int) (y_line[0] + 0.025 * height));
    sprintf (xlabel, "Frequency");
    R_get_text_box (xlabel, &tt1, &tb1, &tl1, &tr1);
    R_standard_color (D_translate_color ("white"));
    R_text (xlabel);
  }

  fac = 0.15;
  R_standard_color (D_translate_color ("white"));
  if (flag == YES)
  {
    R_move_abs ((int) (x_line[2]), (int) (y_line[0] + 0.1 * height));
    sprintf (xlabel, "X-axis scale");
    R_get_text_box (xlabel, &tt1, &tb1, &tl1, &tr1);
    R_text (xlabel);

    for (i = 0; i < no_X; i++)
    {
      R_move_abs ((int) (x_line[2]), (int) (y_line[0] + fac * height));
      sprintf (xlabel, " %d - %6.2f", (int)i, X[i]);
      R_get_text_box (xlabel, &tt1, &tb1, &tl1, &tr1);
      R_text (xlabel);
      fac = fac + 0.05;
    }
  }


  /* find max and min between the varibale to find scale factor */

  if (flag == YES)
  {
    for (i = 0; i < no_X; i++)
    {
      orig_max = MAX (Y[i] * 100, max);
      orig_min = MIN (Y[i] * 100, min);
    }
  }
  max = orig_max;
  min = 0.0;

  /* figure scaling factors and offsets */
  num_cats = no_X;
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

  tic_every = 1;
  tic_unit = 1;

  /*
   * X-AXIS LOOP
   * 
   * loop through category range, drawing a pie-slice and a legend bar on
   * each iteration evenly divisible, a tic-mark on those evenly divisible
   * by tic_unit, and a tic_mark number on those evenly divisible by
   * tic_every
   * 
   */



  for (i = 0; i < no_X; i++)
  {
    if (flag == YES)
      R_standard_color (D_translate_color ("red"));
    else
      R_standard_color (D_translate_color ("green"));
    draw = YES;
    bar_height = (int) (yoffset - yscale * (double) Y[i] * 100);
    /* draw the bar as a box */
    x_box[0] = x_box[1] = xoffset + (i * xscale);
    x_box[2] = x_box[3] = xoffset + (i * xscale);
    y_box[0] = y_box[3] = yoffset;
    y_box[1] = y_box[2] = bar_height;
    if (i == 0)
      R_move_abs ((int) x_box[0], (int) y_box[0]);
    else
      R_move_abs (prev_x, prev_y);
    R_cont_abs ((int) x_box[2], (int) y_box[2]);
    prev_x = (int) x_box[2];
    prev_y = (int) y_box[2];


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
  sprintf (xlabel, "X-AXIS: %s", xlabel1);

  text_height = (b - t) * TEXT_HEIGHT;
  text_width = (r - l) * TEXT_WIDTH * 2.0;
  R_text_size (text_width, text_height);
  R_get_text_box (xlabel, &tt, &tb, &tl, &tr);
  R_move_abs ((int) (l + (r - l) / 2 - (tr - tl) / 2), (int) (b - LABEL_1 * (b - t)));
  R_standard_color ((int) D_translate_color ("white"));
  R_text (xlabel);

  /*
   * DRAW Y-AXIS TIC-MARKS AND NUMBERS
   * 
   * first, figure tic_every and tic_units for the x-axis of the bar-chart.
   * tic_every tells how often to place a tic-number.  tic_unit tells the
   * unit to use in expressing tic-numbers.
   */

  if (flag == YES)
  {
    max_tics = (long) ((y_line[1] - y_line[0]) / YTIC_DIST);

    num_stats = (int) (max - min) + 1;
    tic_every = 10;
    tic_unit = 10;

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
  }

  /* draw the y-axis label */
  sprintf (xlabel, "Y-AXIS: %% Area in tens ");

  text_height = (b - t) * TEXT_HEIGHT;
  text_width = (r - l) * TEXT_WIDTH * 2.0;
  R_text_size (text_width, text_height);
  R_get_text_box (xlabel, &tt, &tb, &tl, &tr);
  R_move_abs ((int) (l + (r - l) / 2 - (tr - tl) / 2), (int) (b - LABEL_2 * (b - t)));
  R_standard_color ((int) D_translate_color ("white"));
  R_text (xlabel);

  /* draw x and y axis lines */
  R_standard_color (D_translate_color ("white"));
  R_polyline_abs (x_line, y_line, 3);

  R_flush ();
  return 0;
}
