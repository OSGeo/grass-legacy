
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

int save_output_maps ()
{
  char buf[512], buf1[512], *mapset;
  int choice, save_opt_menu(), nutrient_opt();
  extern void working_sngl ();
  int G_clear_screen ();
  int copy_map ();

  choice = save_opt_menu ();

  if (choice == 0)
    return 0;
  else
    MAPS_SAVED = YES;

  if (choice == nutrients)
    choice = nutrient_opt ();

  G_clear_screen ();

  mapset = G_ask_cell_new ("Please enter the name of the file to store the maps", buf);

  working_sngl ();

  if (choice == soil_loss)
  {
    printf ("\n\nThe following maps are stored in the current mapset:\n\t%s.ero,\n\t %s.dep and \n\t%s.yield \n", buf, buf, buf);
    sprintf (buf1, "%s.ero", buf);
    copy_map (sed_in, buf1);
    sprintf (buf1, "%s.dep", buf);
    copy_map (sed_gen, buf1);
    sprintf (buf1, "%s.yield", buf);
    copy_map (sed_out, buf1);
  }
  else if (choice == runoff)
  {
    printf ("\n\nThe following maps are stored in the current mapset:\n\t%s.ro_us,\n\t %s.ro_gen and \n\t%s.ro_ds \n", buf, buf, buf);
    sprintf (buf1, "%s.ro_us", buf);
    copy_map (ro_us, buf1);
    sprintf (buf1, "%s.ro_gen", buf);
    copy_map (ro_gen, buf1);
    sprintf (buf1, "%s.ro_ds", buf);
    copy_map (ro_ds, buf1);
  }
  else if (choice == N)
  {
    printf ("\n\nThe following maps are stored in the current mapset:\n\t %s.N_sed_in,\n\t %s.N_sed_out \n %s.N_ro_in,\n\t and %s.N_ro_out \n", buf, buf, buf, buf);
    sprintf (buf1, "%s.N_sed_in", buf);
    copy_map (N_sed_in, buf1);
    sprintf (buf1, "%s.N_sed_out", buf);
    copy_map (N_sed_out, buf1);
    sprintf (buf1, "%s.N_ro_in", buf);
    copy_map (N_ro_in, buf1);
    sprintf (buf1, "%s.N_ro_out", buf);
    copy_map (N_ro_out, buf1);
  }
  else if (choice == P)
  {
    printf ("\n\nThe following maps are stored in the current mapset:\n\t %s.P_sed_in,\n\t %s.P_sed_out \n\t %s.P_ro_in,\n\t and %s.P_ro_out \n", buf, buf, buf, buf);
    sprintf (buf1, "%s.P_sed_in", buf);
    copy_map (P_sed_in, buf1);
    sprintf (buf1, "%s.P_sed_out", buf);
    copy_map (P_sed_out, buf1);
    sprintf (buf1, "%s.P_ro_in", buf);
    copy_map (P_ro_in, buf1);
    sprintf (buf1, "%s.P_ro_out", buf);
    copy_map (P_ro_out, buf1);
  }
  else if (choice == COD)
  {
    printf ("\n\nThe following maps are stored in the current mapset:\n\t and %s.COD_ro_in,\n\t %s.COD_ro_out \n", buf, buf);
    sprintf (buf1, "%s.COD_ro_in", buf);
    copy_map (COD_ro_in, buf1);
    sprintf (buf1, "%s.COD_ro_out", buf);
    copy_map (COD_ro_out, buf1);
  }
  return 0;
}
