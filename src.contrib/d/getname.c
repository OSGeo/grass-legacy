#include "gis.h"
char *
getname (argc, argv, element, desc, n)
    char **argv;
    char *element;
    char *desc;
    int *n;
{
    static char name[128];
    char *mapset;
    int i;

    if (argc)
    {
	*n = 1;
	strcpy (name, argv[0]);
	if (argc > 1 && strcmp (argv[1], "in") == 0)
	{
	    *n = 2;
	    strcat (name, " in ");
	    if (argc > 2)
	    {
		strcat (name, argv[2]);
		*n = 3;
	    }
	}
	if (!G_find_file (element, name, ""))
	{
	    printf ("[%s] - %s file not found\n", name, desc);
	    return NULL;
	}
	return name;
    }
    *n = 0;
    if (!isatty(0)) return NULL;

    mapset = G_ask_old ("", name, element, desc);
    if (mapset == NULL) return NULL;
    strcat (name, " in ");
    strcat (name, mapset);
    return name;
}
