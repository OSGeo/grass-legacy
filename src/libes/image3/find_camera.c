/**************************************************************
* I_find_camera (group)
*
* Find the a camera file in the current group (if it exists)
**************************************************************/
#include "gis.h"

int 
I_find_camera (char *group)
{
    char *element;
    element = (char *) G_malloc (80*sizeof(char));

    if (group == NULL || *group == 0)
	return 0;
    sprintf (element,"group/%s",group);
    return G_find_file (element, "CAMERA" , G_mapset()) != NULL ;
}





