/**************************************************************
* I_find_init (group)
*
* Find the a camera initial expose file in the current group (if it exists)
**************************************************************/
#include "gis.h"

int 
I_find_initial (char *group_name)
{
    char *element;
    element = (char *) G_malloc (80*sizeof(char));

    if (group_name == NULL || *group_name == 0)
	return 0;
    sprintf (element,"group/%s",group_name);
    return G_find_file (element, "INIT_EXP" , G_mapset()) != NULL ;
}





