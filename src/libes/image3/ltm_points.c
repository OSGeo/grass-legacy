/*======================================================================
                             ltm_points.c

======================================================================*/

#include "ortho_image.h"


int I_get_ltm_points_data (Rectify_Group *group)
{
   /* allocate the control points for landsat TM */
   group->points = (Control_Points *) G_malloc (sizeof (Control_Points));

   /* now get the control points */
   get_ltm_points_data(group);

   return 0;
}


int get_ltm_points_data (Rectify_Group *group)
{
Control_Points   *points;
char             msg[100];


    /* make points visiable */
    points = (Control_Points *) group->points;

    /* get the Lat/Lon control points */
    if (!I_get_con_points_ll(group->name, &points->points_ll))
      	points->points_ll.count = 0;

    /* convert the lat/lon to current locatation coordinates */
    convert_from_ll (&points->points_ll, &points->points_temp);

    return 0;
}
