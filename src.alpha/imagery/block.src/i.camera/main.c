/* i.camera */
/* select a camera reference file for a given imagery group */

/* Mike Baba */
/* DBA Systems */
/* Fairfax VA. */ 

#include "gis.h"
#include "camera.h"

static char *intro[]=
{
"This program selects a camera reference file for an imagery group",0
};

main(argc, argv) char *argv[];
{
    static int i;
    char group[40], *location, *mapset, camera[40]; 

    G_gisinit (argv[0]);
    location = G_location();
    mapset = G_mapset();

    for (i=0; intro[i]; i++)
	printf ("%s\n", intro[i]);

    if (!I_ask_group_old  ("Enter group that needs a camera reference file",group))
	exit(0);
    if (!I_ask_camera_old  ("Enter an existing camera reference file to be used with this group",camera))
	exit(0);
    
      /* I_put_camera (camera); */
      I_put_group_camera (group, camera);
   
    printf ("group [%s] in location [%s] mapset [%s] now has camera file [%s]\n", group, location, mapset, camera);
    exit(0);
}

