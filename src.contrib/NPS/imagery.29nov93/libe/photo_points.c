/*======================================================================
                             photo_points.c

======================================================================*/

#include "ortho_image.h"


I_get_photo_points_data (group) 
Rectify_Group *group;
{
   /* allocate the control points for photos */
   group->points = (Control_Points *) G_malloc (sizeof (Control_Points));

   /* now get the control points */
   get_photo_points_data(group);
}


get_photo_points_data(group)
Rectify_Group    *group;
{
Control_Points   *points;
char             msg[100];


    /* make points visiable */
    points = (Control_Points *) group->points;

    /* get the Lat/Lon control points */
    if (!I_get_con_points_ll(group, &points->points_ll))
      	points->points_ll.count = 0;

    /* convert the lat/lon to current locatation coordinates */
    convert_from_ll (&points->points_ll, &points->points_temp);

}







