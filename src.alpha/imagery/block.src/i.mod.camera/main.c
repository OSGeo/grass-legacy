/* Mike Baba */
/* DBA Systems */
/* Fairfax VA. */

#include "camera.h"

static char *intro[]=
{
"This program creates or modifies an camera reference file ",0
};

struct Camera_File_Ref cam_info;

main(argc, argv) char *argv[];
{
    int i;
    int have_old;
    char *group, *location, *mapset, *camera, *file;

    G_gisinit (argv[0]);
    camera = (char *) malloc (40*sizeof (char), 1);
    group  = (char *) malloc (40*sizeof (char), 1);
    
    location = G_location();
    mapset = G_mapset();

    for (i=0; intro[i]; i++)
	printf ("%s\n", intro[i]);

    if (!I_ask_camera_any  ("Enter a new or existing camera reference file ",camera))
	exit(0);
    if (I_find_camera(camera))
          {  have_old = 1;
             I_get_cam_info (camera, &cam_info);
          } 
    mod_cam_info(have_old, &cam_info);
    I_put_cam_info(camera, &cam_info);

    exit(0);
}
