/*
 *                            ltm_auxil.c
 *
 ======================================================================*/

#include "ortho_image.h"

int 
I_get_ltm_auxil_data (Rectify_Group *group)
{
  Auxillary_Ltm  *auxil;
  char            msg[100];   /* warning messages */


  /* alloc the group auxillary data */
  group->auxil = (Auxillary_Ltm *) G_malloc (sizeof (Auxillary_Ltm));

  /* make auxil visiable */
  auxil = (Auxillary_Ltm *) group->auxil;


/** TODO */
    /* get the fiducial points for the imagery group */
 /*  if (I_get_ref_points(group->name, &auxil->points_fid) < -1) {
 */
    /* If FIDUCIAL file exists, but is a BAD format, exit */
 /*    sprintf (msg, "Bad format in FIDUCIAL file for group [%s]\n", group->name);
 *    G_fatal_error (msg);
 *  }
 */

  /* get the group elevation layer infomation */
  if (I_get_group_elev (group->name, &auxil->elev) < -1) {

    /* If ELEVATION file exists, but is a BAD format, exit */
    sprintf (msg, "Bad format in camera file for group [%s]\n", group->name);
    G_fatal_error (msg);
  }


  /* get the SATELLITE info for the group */
  if (I_get_group_sat (group->name, &auxil->satellite) < -1) {   

    /* If SATELLITE file exists, but is a BAD format, exit */
    sprintf (msg, 
	     "Bad format in satellite file for group [%s]\n", group->name);
    G_fatal_error (msg);
  }

  /* get initial satellite exposure station, if any*/
  if (I_get_group_satexp (group->name, &auxil->satellite_expose) < -1) {
    sprintf (msg, 
	     "Bad format in satellite exposusre file for group [%s]\n",
	     group->name);
    G_fatal_error (msg);
  }



  return (1);
}






