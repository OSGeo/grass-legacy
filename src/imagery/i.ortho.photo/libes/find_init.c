/**************************************************************
* I_find_init (group)
*
* Find the a camera initial file in the current group (if it exists)
**************************************************************/
#include "gis.h"

I_find_initial (group)
    char *group;
{
    char *element;
    element = (char *) G_malloc (80*sizeof(char));

    if (group == NULL || *group == 0)
	return 0;
    sprintf (element,"group/%s",group);
    return G_find_file (element, "INIT_EXP" , G_mapset()) != NULL ;
}





