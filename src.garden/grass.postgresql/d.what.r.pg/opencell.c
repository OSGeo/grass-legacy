#include "what.h"

opencell (name, mapset)
char *name;
char *mapset;
{
	char *m;
	int fd;

	m = G_find_cell2 (name, "");
	if (m == NULL)
	{
		fprintf (stderr, "warning: %s - растровая карта не найдена.\n", name);
		return -1;
	}
	if (strlen (m) == 0)
	    strcpy (mapset, G_mapset ());
	else
	    strncpy (mapset, m, strlen(m));
	fd = G_open_cell_old (name, mapset);
	if (fd < 0)
		fprintf (stderr, "warning: не могу открыть [%s] в [%s]\n",
		    name, mapset);
	return fd;
}
