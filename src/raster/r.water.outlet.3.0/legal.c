#include "gis.h"

do_legal (file_name)
char *file_name;
{
	char buf[120];

	if (G_legal_filename (file_name) == -1) {
		sprintf (buf, "map layer [%s] not legal for GRASS\n",
			file_name);
		G_fatal_error (buf);
		exit (1);
	}
}

char *
do_exist (file_name)
char	*file_name;
{
	char buf[120], *file_mapset;

	file_mapset = G_find_cell2 (file_name, "");
	if (file_mapset == NULL)	{
		sprintf (buf, "[%s] map not found\n", file_name);
		G_fatal_error (buf);
		exit (1);
	}
	return (file_mapset);
}
