
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

int usr_display (option)
  int option;
{
  struct Cell_head window;
  char map_name[64], buf[512];
  int choice;
  int G__get_window (), G_set_window (), G_fatal_error ();
  int G_system (), cell_map_opt ();
  int display_new (), draw_scale ();


  G__get_window (&window, "", "WIND", G_mapset ());
  if (G_set_window (&window) == -1)
    G_fatal_error ("Can't set current graphics window");

  if (option == 3)
  {
    G_ask_vector_old ("Vector map to display", map_name);
    sprintf (buf, "d.vect %s color=blue", map_name);
    G_system (buf);
  }
  else if (option == 1 || option == 2)
  {
    choice = cell_map_opt ();
    switch (choice)
    {
    case 1:
      display_new (sed_in, "Erosion in tons", option);
      draw_scale (sed_in, gen_above_max[cur], gen_above_min[cur]);
      break;
    case 2:
      display_new (sed_gen, "Deposition in tons", option);
      draw_scale (sed_gen, within_max[cur], within_min[cur]);
      break;
    case 3:
      display_new (sed_out, "Sediment leaving cell in tons", option);
      draw_scale (sed_out, yield_max[cur], yield_min[cur]);
      break;
    case 4:
      display_new (ro_us, "Runoff from Upstream in inches", option);
      draw_scale (ro_us, ro_us_max[cur], ro_us_min[cur]);
      break;
    case 5:
      display_new (ro_gen, "Runoff generated in inches", option);
      draw_scale (ro_gen, ro_gen_max[cur], ro_gen_min[cur]);
      break;
    case 6:
      display_new (ro_ds, "Runoff to Downstream in inches", option);
      draw_scale (ro_ds, ro_ds_max[cur], ro_ds_min[cur]);
      break;
    case 7:
      display_new (N_sed_in, "Total N in Sediment generated in lbs per ac", option);
      draw_scale (N_sed_in, N_sed_in_max[cur], N_sed_in_min[cur]);
      break;
    case 8:
      display_new (N_sed_out, "Total N in Sediment leaving in lbs per ac", option);
      draw_scale (N_sed_out, N_sed_out_max[cur], N_sed_out_min[cur]);
      break;
    case 9:
      display_new (N_ro_in, "Total N in Runoff generated in lbs per ac", option);
      draw_scale (N_ro_in, N_ro_in_max[cur], N_ro_in_min[cur]);
      break;
    case 10:
      display_new (N_ro_out, "Total N in Runoff leaving in lbs per ac", option);
      draw_scale (N_ro_out, N_ro_out_max[cur], N_ro_out_min[cur]);
      break;
    case 11:
      display_new (P_sed_in, "Total P in Sediment generated in lbs per ac", option);
      draw_scale (P_sed_in, P_sed_in_max[cur], P_sed_in_min[cur]);
      break;
    case 12:
      display_new (P_sed_out, "Total P in Sediment leaving in lbs per ac", option);
      draw_scale (P_sed_out, P_sed_out_max[cur], P_sed_out_min[cur]);
      break;
    case 13:
      display_new (P_ro_in, "Total P in Runoff generated in lbs per ac", option);
      draw_scale (P_ro_in, P_ro_in_max[cur], P_ro_in_min[cur]);
      break;
    case 14:
      display_new (P_ro_out, "Total P in Runoff leaving in lbs per ac", option);
      draw_scale (P_ro_out, P_ro_out_max[cur], P_ro_out_min[cur]);
      break;
    case 15:
      display_new (COD_ro_in, "Total COD in Runoff generated in lbs per ac", option);
      draw_scale (COD_ro_in, COD_ro_in_max[cur], COD_ro_in_min[cur]);
      break;
    case 16:
      display_new (COD_ro_out, "Total COD in Runoff leaving in lbs per ac", option);
      draw_scale (COD_ro_out, COD_ro_out_max[cur], COD_ro_out_min[cur]);
      break;
    case 17:
      G_ask_cell_old ("Cell map to display", map_name);
      if (option == 1)
	sprintf (buf, "d.rast %s", map_name);
      else
	sprintf (buf, "d.rast -o %s", map_name);
      break;
    }
  }
  return 0;
}
