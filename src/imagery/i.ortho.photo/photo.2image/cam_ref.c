#include "globals.h"
/* read the camera reference file for the group */

int 
get_cam_ref (void)
{
    char location[40];
    char mapset[40];
    char buf[1024];
    int stat;

    if (!I_get_cam_ref(group.name, location, mapset))
    {
	sprintf(buf, "Camera Reference File for group [%s] missing\n", group.name);
	goto error;
    }

}

