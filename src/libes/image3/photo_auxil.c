/*======================================================================
                             photo_auxil.c

RETURNS:
   1  all ok
  -1  something wrong

NOTE: Files (like FIDUCIAL or ELEVATION) that dont exist print a warning
      message and set status == -1.   A bad format for the files will
      do a G_fatal_error.  
======================================================================*/

#include "ortho_image.h"

int 
I_get_photo_auxil_data (Rectify_Group *group)
{
  Auxillary_Photo  *auxil;
  int               ret;        /* status from routines to be called */
  int               status;     /* status to pass back */
  char              msg[100];   /* warning messages */


  /* alloc the group auxillary data */
  group->auxil = (Auxillary_Photo *) G_malloc (sizeof (Auxillary_Photo));

  /* make auxil visiable */
  auxil = (Auxillary_Photo *) group->auxil;

  /* initialize return status */
  status = 1;


  /* get the fiducial points for the imagery group */
  ret = I_get_ref_points(group->name, &auxil->points_fid); 
  if (ret == 0) {
    /* If FIDUCIAL doesn't  exist */
    sprintf (msg, "FIDUCIAL file for group [%s] doesn't exist\n", group->name);
    G_warning (msg);
    status = 0;
  }
  else if (ret == -1) {
    /* If FIDUCIAL file exists, but is a BAD format, exit */
    sprintf (msg, "Bad format in FIDUCIAL file for group [%s]\n", group->name);
    G_fatal_error (msg);
  }


  /* get the group elevation layer infomation */
  ret = I_get_group_elev (group->name, &auxil->elev);
  if (ret == -1) {
    /* If ELEVATION doesn't exist */
    sprintf (msg, "ELEVATION file for group [%s] doesn't exist\n", group->name);
    G_warning (msg);
    status = 0;
  }
  else if (ret == -2) {
    /* If ELEVATION file exists, but is a BAD format, exit */
    sprintf (msg, "Bad format in ELEVATION file for group [%s]\n", group->name);
    G_fatal_error (msg);
  }


  /* get the CAMERA info for the group */
  ret = I_get_group_camera (group->name, &auxil->camera);   
  if (ret == -1) {
    /* If CAMERA doesn't exists */
    sprintf (msg, "Camera file for group [%s]\n", group->name);
    G_warning (msg);
    status = 0;
  }
  else if (ret == -2) {
    /* If CAMERA file exists, but is a BAD format, exit */
    sprintf (msg, "Bad format in camera file for group [%s]\n", group->name);
    G_fatal_error (msg);
  }



  /* get initial camera exposure station, if any*/
  if (I_get_group_expose (group->name, &auxil->camera_expose) < -1) {
    sprintf (msg, 
	     "Bad format in initial exposusre station file for group [%s]\n",
	     group->name);
    G_fatal_error (msg);
  }

  return (status);
}






