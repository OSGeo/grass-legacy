#define GLOBAL
#include "global.h"
#include "crs.h" 



I_get_group_auxillary_data (group) 
Rectify_Group *group;
{

   group->auxil = (Auxillary_Photo *) G_malloc (sizeof (Auxillary_Photo));
   get_photo_auxillary_data(group);
}


get_photo_auxillary_data(group)
Rectify_Group    *group;
{
Auxillary_Photo  *auxil;
char              msg[100];


    /* make auxilary visiable */
    auxil = (Auxillary_Photo *) group->auxil;

    /* get the group elevation layer infomation */
    I_get_group_elev (group->name, &auxil->elev);

    /** look for camera info  for this group **/
    if (!I_get_camera_info (group->name, &auxil->camera))
    {   sprintf (msg, "Bad format in camera file for group [%s]\n", group->name);
	G_fatal_error(msg);
    }

    /* get initial camera exposure station, if any*/
    if (I_find_initial(group->name))
    {
       if (!I_get_expose_info (group->name, &auxil->camera_expose))
       {
	  sprintf (msg, "Bad format in initial exposusre station file for group [%s]\n", group->name);
	  G_warning (msg);
       }
    }



    if (group->trans_type == PHOTO){
      if (!I_get_con_points(group->name, &group->points))
      	group->points.count = 0;
      if (!I_get_ref_points(group->name, &auxil->points_fid))
      	auxil->points_fid.count = 0;

/***
      Compute_fiducial_equation();
      Compute_ortho_equation();
****/

    }
}

