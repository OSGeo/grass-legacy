
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
 * The gen_above and with_in for sediment movement were modified to erosion
 * and deposition. The variables cell_sediment[i].gen_above and
 * cell_sediment[i].within are actually represent erosion and deposition
 * at that cell respectively. erosion(variable cell_sediment[i].gen_above)
 * = cell_sediment[i].within deposition(variable cell_sediment[i].within)
 * = (cell_sediment[i].within +
 * cell_sediment[i].gen_above)*cell_sediment[i].deposition/100
 */

#pragma ident "r.agnps.vis v 1.1B <25 Feb 1995>; Copyright (c) 1992-95. Purdue Research Foundation, West   Lafayette, Indiana 47907. All Rights Reserved."

#include "map_gen.h"
#include "window_management.h"

int main (argc, argv)
  char **argv;
  int argc;
{
  int choice, output_option ();
  extern void working_sngl ();
  int G_gisinit (), G_system ();
  int get_inputs (), read_input (), get_mx_mn_inputs (), read_output ();
  int cr_so_ro_maps (), cr_NPCOD_maps (), cr_stats (), wshd_summary ();
  int window_management (), nutrient_opt (), nutrient_attch_opt ();
  int analyze (), save_output_maps (), clean_up ();

  /* initialize G_inist() and GRASS routines */

  G_gisinit (argv[0]);
  get_inputs ();

  /* all newly created maps will be stored in the current mapset */
  this_mapset = G_mapset ();

  read_input (file_name, cur);

  get_mx_mn_inputs (cur);

  read_output (file_name, cur);

  cr_so_ro_maps (cur);

  cr_NPCOD_maps (cur);

  cr_stats ();

  ANALYSIS_DATA = NO;
  MAPS_SAVED = NO;

  for (;;)
  {
    choice = output_option ();

    /*
     * set no user modified maps to save; this is set to YES when user
     * choose the option 5 in the secondary menu
     */

    ANALYSIS = NO;
    usr_modified = NO;
    switch (choice)
    {
    case 1:
      wshd_summary (cur);
      break;
    case 2:
      /* sediment movement */
      window_management (choice);
      break;
    case 3:
      /* nutrient movement */
      NUTRIENT = nutrient_opt ();
      if (NUTRIENT != COD)
	NUT_ATTCH = nutrient_attch_opt ();
      window_management (choice);
      break;
    case 5:
      /* runoff movement */
      window_management (choice);
      break;
    case 6:
      /* Analyze different runs */
      analyze ();
      break;
    case 7:
      /* save the output maps */
      save_output_maps ();
      break;
    case 8:
      /* exit to GRASS */
      G_system ("myg");
      break;
    case 9:
      clean_up ();
      exit (0);
    }
  }



}
