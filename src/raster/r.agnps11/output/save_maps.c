
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

int save_maps (choice)
  int choice;
{

  char buf[512], buf1[512], *mapset;
  int ans, rename_maps ();
  extern void working_sngl ();
  int G_clear_screen (), G_yes ();

  G_clear_screen ();

  if (usr_modified == NO)
    return 0;
  else
  {
    ans = G_yes ("\n\nWould you like to save the maps that you defined recently ", 1);
    if (ans == 0)
    {
      usr_modified = NO;
      return 0;
    }
    else if (ans == 1)
    {
      MAPS_SAVED = YES;
      mapset = G_ask_cell_new ("Please enter the name of the file to store the maps", buf);

      working_sngl ();

      if (choice == soil_loss)
      {
	printf ("\n\nThe following maps are stored in the current mapset:\n\t%s.sed_above,\n\t %s.sed_gen and \n\t%s.sed_yield \n", buf, buf, buf);
	sprintf (buf1, "%s.sed_above", buf);
	rename_maps (sed_above, buf1);
	sprintf (buf1, "%s.sed_gented", buf);
	rename_maps (sed_gented, buf1);
	sprintf (buf1, "%s.sed_yield", buf);
	rename_maps (sed_yield, buf1);
      }
      else if (choice == runoff)
      {
	printf ("\n\nThe following maps are stored in the current mapset:\n\t%s.ro_us,\n\t %s.ro_gen and \n\t%s.ro_ds \n", buf, buf, buf);
	sprintf (buf1, "%s.ro_us", buf);
	rename_maps (usr_ro_us, buf1);
	sprintf (buf1, "%s.ro_gen", buf);
	rename_maps (usr_ro_gen, buf1);
	sprintf (buf1, "%s.ro_ds", buf);
	rename_maps (usr_ro_ds, buf1);
      }
      else if (choice == nutrients)
      {
	if (NUTRIENT == N && NUT_ATTCH == sed)
	{
	  printf ("\n\nThe following maps are stored in the current mapset:\n\t and %s.N_sed_in,\n\t %s.N_sed_out \n", buf, buf);
	  sprintf (buf1, "%s.N_sed_in", buf);
	  rename_maps (usr_N_sed_in, buf1);
	  sprintf (buf1, "%s.N_sed_out", buf);
	  rename_maps (usr_N_sed_out, buf1);
	}
	else if (NUTRIENT == N && NUT_ATTCH == ro)
	{
	  printf ("\n\nThe following maps are stored in the current mapset:\n\t and %s.N_ro_in,\n\t %s.N_ro_out \n", buf, buf);
	  sprintf (buf1, "%s.N_ro_in", buf);
	  rename_maps (usr_N_ro_in, buf1);
	  sprintf (buf1, "%s.N_ro_out", buf);
	  rename_maps (usr_N_ro_out, buf1);
	}
	if (NUTRIENT == P && NUT_ATTCH == sed)
	{
	  printf ("\n\nThe following maps are stored in the current mapset:\n\t and %s.P_sed_in,\n\t %s.P_sed_out \n", buf, buf);
	  sprintf (buf1, "%s.P_sed_in", buf);
	  rename_maps (usr_P_sed_in, buf1);
	  sprintf (buf1, "%s.P_sed_out", buf);
	  rename_maps (usr_P_sed_out, buf1);
	}
	else if (NUTRIENT == P && NUT_ATTCH == ro)
	{
	  printf ("\n\nThe following maps are stored in the current mapset:\n\t and %s.P_ro_in,\n\t %s.P_ro_out \n", buf, buf);
	  sprintf (buf1, "%s.P_ro_in", buf);
	  rename_maps (usr_P_ro_in, buf1);
	  sprintf (buf1, "%s.P_ro_out", buf);
	  rename_maps (usr_P_ro_out, buf1);
	}
	else if (NUTRIENT == COD)
	{
	  printf ("\n\nThe following maps are stored in the current mapset:\n\t and %s.COD_ro_in,\n\t %s.COD_ro_out \n", buf, buf);
	  sprintf (buf1, "%s.COD_ro_in", buf);
	  rename_maps (usr_COD_ro_in, buf1);
	  sprintf (buf1, "%s.COD_ro_out", buf);
	  rename_maps (usr_COD_ro_out, buf1);
	}
      }
    }
  }
  return 0;
}
