#include "gis.h"

ask_layers (file)
    char *file;
{
    FILE *fd;
    char list[400];
    char name[100];
    char *mapset;
    char prompt[1000];

    fd = fopen (file, "w");
    if (!fd)
    {
	perror (file);
	exit (-1);
    }

    printf("You may select one or more map layers to be analyzed\n\n");

    *list = 0;
    while(1)
    {
	if (*list)
	{
	    strcat (list, " ");
	    sprintf (prompt,"selected layers: %s\n\nselect another map layer", list);
	}
	else
	    sprintf (prompt,"select first map layer");

	mapset = G_ask_cell_old (prompt, name);
	if (!mapset)
		break;
	fprintf (fd, "%s %s\n", name, mapset);
	strcat (list, name);
    }

    fclose (fd);
}
