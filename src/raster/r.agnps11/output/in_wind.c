
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

int in_wind (cell_num, j)
  int cell_num;
  int j;
{
  int text_height;
  int text_width;
  char window_name[64];
  char title[512];
  int tt, tb, tl, tr;
  int t, b, l, r;
  int R_open_driver (), D_get_cur_wind (), G_fatal_error (), D_set_cur_wind ();
  int R_font (), D_get_screen_window (), R_text_size (), R_get_text_box ();
  int R_move_abs (), R_standard_color (), D_translate_color (), R_text ();
  int R_flush (), R_close_driver ();

  /* set up the graphics driver and initialize its color-table */
  R_open_driver ();

  if (D_get_cur_wind (window_name))
    G_fatal_error ("No current window");

  if (D_set_cur_wind (window_name))
    G_fatal_error ("Current window not available");

  /* draw a title for */

  if (cell_num < no_cells[j] + 2)
    sprintf (title, "Input for the Cell Number %d", cell_num);
  else
    sprintf (title, "Average Input for the selected area");

  R_font ("romans");
  D_get_screen_window (&t, &b, &l, &r);
  text_height = (b - t) * 0.04;
  text_width = (r - l) * 0.05 * 0.7;
  R_text_size (text_width, text_height);
  R_get_text_box (title, &tt, &tb, &tl, &tr);
  R_move_abs ((int) (l + (r - l) / 2 - (tr - tl) / 2), (int) (t - 10 + (b - t) * 0.07));
  R_standard_color (D_translate_color ("white"));
  R_text (title);

  text_width = (r - l) * 0.05 * 0.7;
  R_text_size (text_width, text_height);
  if (cell_num < no_cells[j] + 2)
    sprintf (title, "Recieving Cell number          = %d", ag_inp[j][cell_num - 1].rcell_num / 1000);
  else
    sprintf (title, " ");
  R_get_text_box (title, &tt, &tb, &tl, &tr);
  /*
   * R_move_abs((int)(l+(r-l)/2-(tr-tl)/2),(int)(t+10+(b-t)*0.07));
   */
  R_move_abs ((int) (l + 2), (int) (t + 10 + (b - t) * 0.07));
  R_text (title);

  /*
   * text_width = (r-l)*0.05*0.7; R_text_size(text_width,text_height);
   */
  sprintf (title, "Curve Number                   = %d ", ag_inp[j][cell_num - 1].cn);
  R_get_text_box (title, &tt, &tb, &tl, &tr);
  /*
   * R_move_abs((int)(l+(r-l)/2-(tr-tl)/2),(int)(t+30+(b-t)*0.07));
   */
  R_move_abs ((int) (l + 2), (int) (t + 30 + (b - t) * 0.07));
  R_text (title);


  sprintf (title, "Average Slope                  = %5.1f %%", ag_inp[j][cell_num - 1].slope_pct);
  R_get_text_box (title, &tt, &tb, &tl, &tr);
  /*
   * R_move_abs((int)(l+(r-l)/2-(tr-tl)/2),(int)(t+50+(b-t)*0.07));
   */
  R_move_abs ((int) (l + 2), (int) (t + 50 + (b - t) * 0.07));
  R_text (title);

  sprintf (title, "Average Channel Slope          = %5.1f %%", ag_inp[j][cell_num - 1].chnl_slope);
  R_get_text_box (title, &tt, &tb, &tl, &tr);
  R_move_abs ((int) (l + 2), (int) (t + 70 + (b - t) * 0.07));
  R_text (title);

  sprintf (title, "Mannings 'n'                   = %4.3f", ag_inp[j][cell_num - 1].man_n);
  R_get_text_box (title, &tt, &tb, &tl, &tr);
  R_move_abs ((int) (l + 2), (int) (t + 90 + (b - t) * 0.07));
  R_text (title);

  sprintf (title, "USLE K factor                  = %4.3f", ag_inp[j][cell_num - 1].k_val);
  R_get_text_box (title, &tt, &tb, &tl, &tr);
  R_move_abs ((int) (l + 2), (int) (t + 110 + (b - t) * 0.07));
  R_text (title);

  sprintf (title, "USLE C factor                  = %4.3f", ag_inp[j][cell_num - 1].c_fac);
  R_get_text_box (title, &tt, &tb, &tl, &tr);
  R_move_abs ((int) (l + 2), (int) (t + 130 + (b - t) * 0.07));
  R_text (title);

  sprintf (title, "USLE P factor                  = %5.3f", ag_inp[j][cell_num - 1].p_fac);
  R_get_text_box (title, &tt, &tb, &tl, &tr);
  R_move_abs ((int) (l + 2), (int) (t + 150 + (b - t) * 0.07));
  R_text (title);

  sprintf (title, "Surface condition constant     = %5.3f", ag_inp[j][cell_num - 1].surf_cond);
  R_get_text_box (title, &tt, &tb, &tl, &tr);
  R_move_abs ((int) (l + 2), (int) (t + 170 + (b - t) * 0.07));
  R_text (title);

  sprintf (title, "Aspect (flow direction)        = %d", ag_inp[j][cell_num - 1].aspect);
  R_get_text_box (title, &tt, &tb, &tl, &tr);
  R_move_abs ((int) (l + 2), (int) (t + 190 + (b - t) * 0.07));
  R_text (title);

  sprintf (title, "Fertilizer application factor  = %d", ag_inp[j][cell_num - 1].fert_fac);
  R_get_text_box (title, &tt, &tb, &tl, &tr);
  R_move_abs ((int) (l + 2), (int) (t + 210 + (b - t) * 0.07));
  R_text (title);

  sprintf (title, "Fertilizer availablity factor  = %d", ag_inp[j][cell_num - 1].fert_avl_fac);
  R_get_text_box (title, &tt, &tb, &tl, &tr);
  R_move_abs ((int) (l + 2), (int) (t + 230 + (b - t) * 0.07));
  R_text (title);

  sprintf (title, "Slope Shape Factor             = %d", ag_inp[j][cell_num - 1].slope_shape);
  R_get_text_box (title, &tt, &tb, &tl, &tr);
  R_move_abs ((int) (l + 2), (int) (t + 250 + (b - t) * 0.07));
  R_text (title);

  sprintf (title, "Slope Length Factor            = %d", ag_inp[j][cell_num - 1].slope_ln);
  R_get_text_box (title, &tt, &tb, &tl, &tr);
  R_move_abs ((int) (l + 2), (int) (t + 270 + (b - t) * 0.07));
  R_text (title);

  sprintf (title, "Channel Side Slope             = %5.1f%%", ag_inp[j][cell_num - 1].chnl_side_slope);
  R_get_text_box (title, &tt, &tb, &tl, &tr);
  R_move_abs ((int) (l + 2), (int) (t + 290 + (b - t) * 0.07));
  R_text (title);

  sprintf (title, "Soil Texture                   = %d", ag_inp[j][cell_num - 1].texture);
  R_get_text_box (title, &tt, &tb, &tl, &tr);
  R_move_abs ((int) (l + 2), (int) (t + 310 + (b - t) * 0.07));
  R_text (title);

  sprintf (title, "COD Factor                     = %d", ag_inp[j][cell_num - 1].cod_fac);
  R_get_text_box (title, &tt, &tb, &tl, &tr);
  R_move_abs ((int) (l + 2), (int) (t + 330 + (b - t) * 0.07));
  R_text (title);

  R_flush ();
  R_close_driver ();
  return 0;
}
