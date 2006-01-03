/* select_camera */
/* select a camera reference file for a given imagery group */

#define  MAIN   1
#include <stdlib.h>
#include "orthophoto.h"
#include "globals.h"

int 
main (int argc, char *argv[])
{
    static int i;
    static int have_old;
    char *group, *location, *mapset, *camera; 

    location = (char *) G_malloc (80*sizeof (char));
    mapset   = (char *) G_malloc (80*sizeof (char));
    group    = (char *) G_malloc (80*sizeof (char));
    camera   = (char *) G_malloc (80*sizeof (char));

    /* initialize */
    G_gisinit (argv[0]);
    location = G_location();
    mapset   = G_mapset();

    /* check args */
    if (argc != 2)
    {
        fprintf (stderr, "Usage: %s group\n", argv[0]);
        exit(1);
    }

    /* get group for argv */
    group = argv[1];

    /* select the camera to use */
    if (!I_ask_camera_any  ("Enter a camera reference file to be used with this imagery group",camera))
	exit(0);
    
    /* I_put_camera (camera); */
    I_put_group_camera (group, camera);
   
    fprintf (stderr, "group [%s] in location [%s] mapset [%s] now has camera file [%s]\n", group, location, mapset, camera);


    /* show the camera infor for modification */
    if (I_find_camera(camera))
          {  have_old = 1;
             I_get_cam_info (camera, &cam_info);
          }
    mod_cam_info(have_old, &cam_info);
    I_put_cam_info(camera, &cam_info);



    exit(0);
}

