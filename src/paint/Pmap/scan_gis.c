#include "gis.h"
scan_gis (element, desc, key, data, name, mapset, gobble)
    char *element;
    char *desc;
    char *key;
    char *data;
    char *name;
    char *mapset;
{
    char *ms;
    char tname[50], tmapset[50];


    *tmapset = 0;
    *tname = 0;
    if (sscanf (data, "%s in %s", tname, tmapset) != 2
    &&  sscanf (data, "%s %s", tname, tmapset) < 1)
    {
	error (key,data,"illegal request");
	if (gobble)
	    gobble_input();
	return 0;
    }

    if (strcmp(tname,"list") == 0)
    {
	if (isatty(0))
	    G_list_element (element, desc, tmapset, (int (*)()) NULL);
	reject();
	return 0;
    }

    ms = G_find_file (element, tname, tmapset);
    if (ms == NULL)
    {
	error (key, data, "not found");
	if (gobble)
	    gobble_input();
	return 0;
    }
    strcpy (mapset, ms);
    strcpy (name, tname);
    return 1;
}
