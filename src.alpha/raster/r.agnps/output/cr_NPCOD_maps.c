
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

int cr_NPCOD_maps (j)
  int j;
{
  char buf[512];
  char *emalloc ();
  struct Colors N_sed_in_colors, N_sed_out_colors;
  struct Colors N_ro_in_colors, N_ro_out_colors;
  int make_grn_yel_red ();
  int G_init_colors (), G_set_color (), G_write_colors (), G_free_colors ();

  printf ("Creating Color structures for N associated Nutrient maps\n");

  /* initiate the colors structures for the maps */
  G_init_colors (&N_sed_in_colors);
  G_init_colors (&N_sed_out_colors);
  G_init_colors (&N_ro_in_colors);
  G_init_colors (&N_ro_out_colors);

  make_grn_yel_red (&N_sed_in_colors, (int) (N_sed_in_min[j] * sig_fac),
		    (int) (N_sed_in_max[j] * sig_fac));
  make_grn_yel_red (&N_sed_out_colors, (int) (N_sed_out_min[j] * sig_fac),
		    (int) (N_sed_out_max[j] * sig_fac));
  make_grn_yel_red (&N_ro_in_colors, (int) (N_ro_in_min[j] * sig_fac),
		    (int) (N_ro_in_max[j] * sig_fac));
  make_grn_yel_red (&N_ro_out_colors, (int) (N_ro_out_min[j] * sig_fac),
		    (int) (N_ro_out_max[j] * sig_fac));


  G_set_color ((CELL) 0, 0, 0, 0, &N_sed_in_colors);
  G_set_color ((CELL) 0, 0, 0, 0, &N_sed_out_colors);
  G_set_color ((CELL) 0, 0, 0, 0, &N_ro_in_colors);
  G_set_color ((CELL) 0, 0, 0, 0, &N_ro_out_colors);

  if (j == cur)
  {
    G_write_colors (N_sed_in, this_mapset, &N_sed_in_colors);
    G_write_colors (N_sed_out, this_mapset, &N_sed_out_colors);
    G_write_colors (N_ro_in, this_mapset, &N_ro_in_colors);
    G_write_colors (N_ro_out, this_mapset, &N_ro_out_colors);
  }
  if (j == prev)
  {
    sprintf (buf, "%s_prev", N_sed_in);
    G_write_colors (buf, this_mapset, &N_sed_in_colors);
    sprintf (buf, "%s_prev", N_sed_out);
    G_write_colors (buf, this_mapset, &N_sed_out_colors);
    sprintf (buf, "%s_prev", N_ro_in);
    G_write_colors (buf, this_mapset, &N_ro_in_colors);
    sprintf (buf, "%s_prev", N_ro_out);
    G_write_colors (buf, this_mapset, &N_ro_out_colors);
  }


  G_free_colors (&N_sed_in_colors);
  G_free_colors (&N_sed_out_colors);
  G_free_colors (&N_ro_in_colors);
  G_free_colors (&N_ro_out_colors);

  printf ("Creating Color Sturctures for P associated Nutrient maps\n");

  /* initiate the colors structures for the maps */

  G_init_colors (&N_sed_in_colors);
  G_init_colors (&N_sed_out_colors);
  G_init_colors (&N_ro_in_colors);
  G_init_colors (&N_ro_out_colors);

  make_grn_yel_red (&N_sed_in_colors, (int) (P_sed_in_min[j] * sig_fac),
		    (int) (P_sed_in_max[j] * sig_fac));
  make_grn_yel_red (&N_sed_out_colors, (int) (P_sed_out_min[j] * sig_fac),
		    (int) (P_sed_out_max[j] * sig_fac));
  make_grn_yel_red (&N_ro_in_colors, (int) (P_ro_in_min[j] * sig_fac),
		    (int) (P_ro_in_max[j] * sig_fac));
  make_grn_yel_red (&N_ro_out_colors, (int) (P_ro_out_min[j] * sig_fac),
		    (int) (P_ro_out_max[j] * sig_fac));

  G_set_color ((CELL) 0, 0, 0, 0, &N_sed_in_colors);
  G_set_color ((CELL) 0, 0, 0, 0, &N_sed_out_colors);
  G_set_color ((CELL) 0, 0, 0, 0, &N_ro_in_colors);
  G_set_color ((CELL) 0, 0, 0, 0, &N_ro_out_colors);

  if (j == cur)
  {
    G_write_colors (P_sed_in, this_mapset, &N_sed_in_colors);
    G_write_colors (P_sed_out, this_mapset, &N_sed_out_colors);
    G_write_colors (P_ro_in, this_mapset, &N_ro_in_colors);
    G_write_colors (P_ro_out, this_mapset, &N_ro_out_colors);
  }
  if (j == prev)
  {
    sprintf (buf, "%s_prev", P_sed_in);
    G_write_colors (buf, this_mapset, &N_sed_in_colors);
    sprintf (buf, "%s_prev", P_sed_out);
    G_write_colors (buf, this_mapset, &N_sed_out_colors);
    sprintf (buf, "%s_prev", P_ro_in);
    G_write_colors (buf, this_mapset, &N_ro_in_colors);
    sprintf (buf, "%s_prev", P_ro_out);
    G_write_colors (buf, this_mapset, &N_ro_out_colors);
  }

  G_free_colors (&N_sed_in_colors);
  G_free_colors (&N_sed_out_colors);
  G_free_colors (&N_ro_in_colors);
  G_free_colors (&N_ro_out_colors);

  printf ("Creating Color Structures for COD associated Nutrient maps\n");

  /* initiate the colors structures for the maps */
  G_init_colors (&N_ro_in_colors);
  G_init_colors (&N_ro_out_colors);

  make_grn_yel_red (&N_ro_in_colors, (int) (COD_ro_in_min[j] * sig_fac),
		    (int) (COD_ro_in_max[j] * sig_fac));
  make_grn_yel_red (&N_ro_out_colors, (int) (COD_ro_out_min[j] * sig_fac),
		    (int) (COD_ro_out_max[j] * sig_fac));

  G_set_color ((CELL) 0, 0, 0, 0, &N_ro_in_colors);
  G_set_color ((CELL) 0, 0, 0, 0, &N_ro_out_colors);

  if (j == cur)
  {
    G_write_colors (COD_ro_in, this_mapset, &N_ro_in_colors);
    G_write_colors (COD_ro_out, this_mapset, &N_ro_out_colors);
  }
  if (j == prev)
  {
    sprintf (buf, "%s_prev", COD_ro_in);
    G_write_colors (buf, this_mapset, &N_ro_in_colors);
    sprintf (buf, "%s_prev", COD_ro_out);
    G_write_colors (buf, this_mapset, &N_ro_out_colors);
  }

  G_free_colors (&N_ro_in_colors);
  G_free_colors (&N_ro_out_colors);

  return 0;
}
