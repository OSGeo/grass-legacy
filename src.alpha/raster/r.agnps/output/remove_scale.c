
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
#define	left	5
#define	bot	5

/* this routine is to erase the scale in a window */

int remove_scale ()
{
  int t, b, l, r;
  int i, box_size;
  int R_open_driver (), D_get_screen_window (), R_standard_color ();
  int D_translate_color (), R_move_abs (), R_box_rel (), R_close_driver ();

  R_open_driver ();

  D_get_screen_window (&t, &b, &l, &r);
  t = b - bot - 35;
  b = b - bot;
  l = l + left;

  box_size = (int) (r - l - 5) / 11;

  R_standard_color (D_translate_color ("black"));
  R_move_abs (l, b - 30);
  R_box_rel (r - l, (t - 25 - (b - 30)));

  R_move_abs (l, b);
  R_box_rel (box_size, -box_size);

  for (i = 0; i < 4; i++)
  {
    R_move_abs (l + (i + 1) * box_size, b);
    R_box_rel (box_size, -box_size);
  }


  R_move_abs (l + 5 * box_size, b);
  R_box_rel (box_size, -box_size);

  for (i = 0; i < 4; i++)
  {
    R_move_abs (l + (i + 6) * box_size, b);
    R_box_rel (box_size, -box_size);
  }

  R_move_abs (l + 10 * box_size, b);
  R_box_rel (box_size, -box_size);

  R_close_driver ();

  return 0;
}
