#include "gis.h"
get_map_name (name)
    char *name;
{
    while(G_ask_cell_any ("Enter name of map to be created", name) == NULL)
    {
	if (G_yes("Quit without creating a map?? ", -1))
		return 0;
    }
    return 1;
}
