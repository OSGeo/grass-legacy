#include "what.h"

opencell (fullname, name, mapset)
char *fullname;
char *name;
char *mapset;
{
	char *m;
	int fd;

	strcpy (name, fullname);
	m = G_find_cell2 (name, "");
	if (m == NULL)
	{
		fprintf (stderr, "warning: %s - raster file not found\n", name);
		return -1;
	}
	if (strlen (m) == 0)
	    strcpy (mapset, G_mapset ());
	else
	    strcpy (mapset, m);
	fd = G_open_cell_old (name, mapset);
	if (fd < 0)
		fprintf (stderr, "warning: unable to open [%s] in [%s]\n",
		    name, mapset);
	return fd;
}
