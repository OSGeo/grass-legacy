#include "list.h"

char *
find (n, name, mapsets)
    char *name;
    char *mapsets;
{
    char *mapset;
    mapset =  G_find_file (list[n].element[0], name, mapsets);
    if (mapset)
    {
	char temp[100];
	sscanf (name, "%s", temp);
	strcpy (name, temp);
    }
    return mapset;
}
