
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
 * clean_up()
 * 
 * removes all the temporary maps and files created by the vaious tools
 * 
 */

#include "map_gen.h"

int clean_up ()
{
  char buf[512];
  extern void working_sngl ();
  int G_system ();

  if (!MAPS_SAVED)
  {
    sprintf (buf, "g.remove rast=%s > /tmp/$$", cell_num_map->p);
    G_system (buf);
  }

  working_sngl ();

  sprintf (buf, "g.remove rast=Sed_in,Sed_gen,Sed_out > /tmp/$$");
  G_system (buf);

  sprintf (buf, "g.remove rast=ro_us,ro_gen,ro_ds > /tmp/$$");
  G_system (buf);

  sprintf (buf, "g.remove rast=usr_ro_us,usr_ro_gen,usr_ro_ds > /tmp/$$");
  G_system (buf);

  sprintf (buf, "g.remove rast=usr_sed_above,usr_sed_gen,usr_sed_yield > /tmp/$$");
  G_system (buf);

  sprintf (buf, "g.remove rast=N_sed_in,N_sed_out,N_ro_in,N_ro_out > /tmp/$$");
  G_system (buf);

  sprintf (buf, "g.remove rast=P_sed_in,P_sed_out,P_ro_in,P_ro_out > /tmp/$$");
  G_system (buf);

  sprintf (buf, "g.remove rast=COD_ro_in,COD_ro_out > /tmp/$$");
  G_system (buf);

  sprintf (buf, "g.remove rast=usr_N_sed_in,usr_N_sed_out,usr_N_ro_in,usr_N_ro_out > /tmp/$$");
  G_system (buf);

  sprintf (buf, "g.remove rast=usr_P_sed_in,usr_P_sed_out,usr_P_ro_in,usr_P_ro_out > /tmp/$$");
  G_system (buf);

  sprintf (buf, "g.remove rast=usr_COD_ro_in,usr_COD_ro_out > /tmp/$$");
  G_system (buf);

  sprintf (buf, "g.remove rast=MASK > /tmp/$$");
  G_system (buf);

  sprintf (buf, "g.remove rast=Sed_in_prev,Sed_gen_prev,Sed_out_prev > /tmp/$$");
  G_system (buf);

  sprintf (buf, "g.remove rast=ro_us_prev=ro_gen_prev=ro_ds_prev > /tmp/$$");
  G_system (buf);

  sprintf (buf, "g.remove rast=usr_ro_us_prev,usr_ro_gen_prev,usr_ro_ds_prev > /tmp/$$");
  G_system (buf);

  sprintf (buf, "g.remove rast=usr_sed_above_prev,usr_sed_gen_prev,usr_sed_yield_prev > /tmp/$$");
  G_system (buf);

  sprintf (buf, "g.remove rast=N_sed_in_prev,N_sed_out_prev,N_ro_in_prev,N_ro_out_prev > /tmp/$$");
  G_system (buf);

  sprintf (buf, "g.remove rast=P_sed_in_prev,P_sed_out_prev,P_ro_in_prev,P_ro_out_prev > /tmp/$$");
  G_system (buf);

  sprintf (buf, "g.remove rast=COD_ro_in_prev,COD_ro_out_prev > /tmp/$$");
  G_system (buf);

  sprintf (buf, "g.remove rast=usr_N_sed_in_prev,usr_N_sed_out_prev,usr_N_ro_in_prev,usr_N_ro_out_prev > /tmp/$$");
  G_system (buf);

  sprintf (buf, "g.remove rast=usr_P_sed_in_prev,usr_P_sed_out_prev,usr_P_ro_in_prev,usr_P_ro_out_prev > /tmp/$$");
  G_system (buf);

  sprintf (buf, "g.remove rast=usr_COD_ro_in_prev,usr_COD_ro_out_prev > /tmp/$$");
  G_system (buf);
  return 0;
}
