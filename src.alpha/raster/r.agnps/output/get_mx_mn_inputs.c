
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
#define MAX(x,y)             (( x>y ) ? x : y)
#define MIN(x,y)             (( x<y ) ? x : y)

int get_mx_mn_inputs (j)
  int j;			/* read current or previous run data sets */
{
  int i;

  /* initialize the max and min values */
  cn[j].mx = 0.0;
  cn[j].mn = 10000.0;
  slope_pct[j].mx = 0.0;
  slope_pct[j].mn = 10000.0;
  slope_ln[j].mx = 0.0;
  slope_ln[j].mn = 10000.0;
  k_val[j].mx = 0.0;
  k_val[j].mn = 10000.0;
  c_fac[j].mx = 0.0;
  c_fac[j].mn = 10000.0;
  fert_fac[j].mx = 0.0;
  fert_fac[j].mn = 10000.0;

  printf ("Finding Max and Min for various inputs \n");

  for (i = 0; i < no_cells[j]; i++)
  {

    cn[j].mx = MAX (ag_inp[j][i].cn, cn[j].mx);
    cn[j].mn = MIN (ag_inp[j][i].cn, cn[j].mn);

    slope_pct[j].mx = MAX (ag_inp[j][i].slope_pct, slope_pct[j].mx);
    slope_pct[j].mn = MIN (ag_inp[j][i].slope_pct, slope_pct[j].mn);

    slope_ln[j].mx = MAX (ag_inp[j][i].slope_ln, slope_ln[j].mx);
    slope_ln[j].mn = MIN (ag_inp[j][i].slope_ln, slope_ln[j].mn);

    k_val[j].mx = MAX (ag_inp[j][i].k_val, k_val[j].mx);
    k_val[j].mn = MIN (ag_inp[j][i].k_val, k_val[j].mn);

    c_fac[j].mx = MAX (ag_inp[j][i].c_fac, c_fac[j].mx);
    c_fac[j].mn = MIN (ag_inp[j][i].c_fac, c_fac[j].mn);

    fert_fac[j].mx = MAX (ag_inp[j][i].fert_fac, fert_fac[j].mx);
    fert_fac[j].mn = MIN (ag_inp[j][i].fert_fac, fert_fac[j].mn);

  }
  return 0;
}
