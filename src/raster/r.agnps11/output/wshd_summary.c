
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

int wshd_summary (j)
  int j;
{

  char buf[512], buf1[512], buf2[512], buf3[512], buf4[512];
  char buf5[512], buf6[512], buf7[512], buf8[512], buf9[512];
  char buf10[512], buf11[512], buf12[512], buf13[512], buf14[512];
  char buf15[512];
  int V_clear (), V_line (), V_call ();

  V_clear ();
  V_line (1, "		Watershed Summary");
  sprintf (buf, "Watershed Studied\t %s", wshd_des[j]);
  V_line (3, buf);
  sprintf (buf1, "The area of the watershed is %30d", total_area[j]);
  V_line (4, buf1);
  sprintf (buf2, "The area of each cell is %34.2f acres", cell_area[j]);
  V_line (5, buf2);
  sprintf (buf3, "The characteristic storm precipitation is %17.2f inches", rainfall[j]);
  V_line (6, buf3);
  sprintf (buf4, "The strom energy-intensity value is %23.0f", ei[j]);
  V_line (7, buf4);
  V_line (9, " 		Values at the Watershed Outlet");
  sprintf (buf5, "Cell number%47d", outlet_cell[j]);
  V_line (10, buf5);
  sprintf (buf6, "Runoff Volume%46.1f inches", ro_vol[j]);
  V_line (11, buf6);
  sprintf (buf7, "Peak runoff rate%43.0f cfs", peak_ro_rate[j]);
  V_line (12, buf7);
  sprintf (buf8, "Total Nitrogen in sediment%33.2f lbs/acre", tot_N_sed[j]);
  V_line (13, buf8);
  sprintf (buf9, "Total soluble Nitrogen in runoff%27.2f lbs/acre", tot_N_ro[j]);
  V_line (14, buf9);
  sprintf (buf10, "Soluble Nitrogen concentration in runoff%19.2f ppm", sol_N_conc_ro[j]);
  V_line (15, buf10);
  sprintf (buf11, "Total Phosphorus in sediment%31.2f lbs/acre", tot_P_sed[j]);
  V_line (16, buf11);
  sprintf (buf12, "Total soluble Phosphorus in runoff%25.2f lbs/acre", tot_P_ro[j]);
  V_line (17, buf12);
  sprintf (buf13, "Soluble Phosphorus concentration in runoff%17.2f ppm", sol_P_conc_ro[j]);
  V_line (18, buf13);
  sprintf (buf14, "Total soluble chemical oxygen demand%23.2f lbs/acre", tot_COD_ro[j]);
  V_line (19, buf14);
  sprintf (buf15, "Soluble chemical oxygen demand concentration in runoff%5.0f ppm", sol_COD_conc_ro[j]);
  V_line (20, buf15);

  if (!V_call ())
    exit (1);

  /* Sediment Summary */

  V_clear ();
  V_line (1, "			Watershed Sediment Summary");
  sprintf (buf, "\t      Erosion\t  Delivery  Enrichment  Mean  Weighted");
  V_line (3, buf);
  sprintf (buf1, "Particle  Upland  Channel Ratio     Ratio       Conc    Yield  Yield");
  V_line (4, buf1);
  sprintf (buf2, "type       (t/a)   (t/a)     (percent)          (ppm)    (t/a) (tons)");
  V_line (5, buf2);
  sprintf (buf3, "CLAY\t%7.2f\t%7.2f\t%5d\t%8d%14.2f%7.2f%8.2f", wshd_sed[j].upland[0], wshd_sed[j].channel[0], wshd_sed[j].del_ratio[0], wshd_sed[j].enrich_ratio[0], wshd_sed[j].mean_conc[0], wshd_sed[j].weighted_yield[0], wshd_sed[j].yield[0]);
  V_line (7, buf3);
  sprintf (buf4, "SILT\t%7.2f\t%7.2f\t%5d\t%8d%14.2f%7.2f%8.2f", wshd_sed[j].upland[1], wshd_sed[j].channel[1], wshd_sed[j].del_ratio[1], wshd_sed[j].enrich_ratio[1], wshd_sed[j].mean_conc[1], wshd_sed[j].weighted_yield[1], wshd_sed[j].yield[1]);
  V_line (8, buf4);
  sprintf (buf5, "SAGG\t%7.2f\t%7.2f\t%5d\t%8d%14.2f%7.2f%8.2f", wshd_sed[j].upland[2], wshd_sed[j].channel[2], wshd_sed[j].del_ratio[2], wshd_sed[j].enrich_ratio[2], wshd_sed[j].mean_conc[2], wshd_sed[j].weighted_yield[2], wshd_sed[j].yield[2]);
  V_line (9, buf5);
  sprintf (buf6, "LAGG\t%7.2f\t%7.2f\t%5d\t%8d%14.2f%7.2f%8.2f", wshd_sed[j].upland[3], wshd_sed[j].channel[3], wshd_sed[j].del_ratio[3], wshd_sed[j].enrich_ratio[3], wshd_sed[j].mean_conc[3], wshd_sed[j].weighted_yield[3], wshd_sed[j].yield[3]);
  V_line (10, buf6);
  sprintf (buf7, "SAND\t%7.2f\t%7.2f\t%5d\t%8d%14.2f%7.2f%8.2f", wshd_sed[j].upland[4], wshd_sed[j].channel[4], wshd_sed[j].del_ratio[4], wshd_sed[j].enrich_ratio[4], wshd_sed[j].mean_conc[4], wshd_sed[j].weighted_yield[4], wshd_sed[j].yield[4]);
  V_line (11, buf7);
  sprintf (buf8, "TOTAL\t%7.2f\t%7.2f\t%5d\t%8d%14.2f%7.2f%8.2f", wshd_sed[j].upland[5], wshd_sed[j].channel[5], wshd_sed[j].del_ratio[5], wshd_sed[j].enrich_ratio[5], wshd_sed[j].mean_conc[5], wshd_sed[j].weighted_yield[5], wshd_sed[j].yield[5]);
  V_line (13, buf8);


  if (!V_call ())
    exit (1);

  return 0;
}
