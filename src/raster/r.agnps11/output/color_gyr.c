
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
 * make_grn_yel_red(pcolr, min, max)
 * 
 * pcolr: color structures min: minimum cell value max: maximum cell value
 * 
 * creates color structure color ranging from green to yellow to red between
 * minimum and maximum cell values.
 */


#include "gis.h"
int make_grn_yel_red (pcolr, min, max)
  struct Colors *pcolr;
  CELL min, max;
{
  int i, j;
  int num;
  int n;
  int red, grn, blu;
  int G_init_colors (), G_set_color ();

  G_init_colors (pcolr);
  if (max < min)
    return -1;

  if (min == 1)
    min = 0;
  if (max == -1)
    max = 0;
  num = max - min + 1;

  n = num / 2;

  grn = 256;
  blu = 0;
  for (i = 1; i <= n; i++)
  {
    red = ((float) i / (float) n) * 256;
    G_set_color ((CELL) (i + min), red, grn, blu, pcolr);
  }

  red = 256;
  blu = 0;
  j = 0;
  for (; i < num; i++)
  {
    grn = 256 - ((float) (j++) / (float) n) * 256;
    G_set_color ((CELL) (i + min), red, grn, blu, pcolr);
  }
  G_set_color ((CELL) (0), 256, 256, 256, pcolr);
  return 1;
}
