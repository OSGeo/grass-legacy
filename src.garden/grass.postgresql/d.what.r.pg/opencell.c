#include <string.h>
#include "what.h"
#include "glocale.h"

int opencell (name, mapset)
char *name;
char *mapset;
{
	char *m;
	int fd;

	m = G_find_cell2 (name, "");
	if (m == NULL)
	{
		fprintf (stderr, _("warning: %s - raster map not found.\n"), name);
		return -1;
	}
	if (strlen (m) == 0)
	    strcpy (mapset, G_mapset ());
	else
	    strncpy (mapset, m, strlen(m));
	fd = G_open_cell_old (name, mapset);
	if (fd < 0)
		fprintf (stderr, _("warning: can not open [%s] in [%s]\n"),
		    name, mapset);
	return fd;
}
