#include <stdio.h>
#include "site.h"

#define NITEMS 5

get_site_list (site_list, file)

	SITE_LIST *site_list;
	char *file;
{
	FILE *fd;
	int stat;

	if (!(fd = fopen(file,"r")))
	{
		perror (file);
		return 0;
	}

	stat = read_site_list (site_list, fd);

	fclose (fd);

	return stat;
}

read_site_list (site_list, fd)

	SITE_LIST *site_list;
	FILE *fd;
{
	int id;
	int north;
	int east;
	char buf[200];
	char *desc;
	char *items[NITEMS];
	int n;

	free_site_list(site_list);
	initialize_site_list(site_list);

	while (G_getl (buf, sizeof buf, fd))
	{
		n = parse (buf, items, NITEMS, "|");
		if (strcmp(items[0],"name") == 0)
		{
			scopy (site_list->name, items[1], sizeof(site_list->name));
			continue;
		}
		if (strcmp(items[0],"desc") == 0)
		{
			scopy (site_list->desc, items[1], sizeof(site_list->desc));
			continue;
		}
		if ((strcmp(items[0],"point") == 0) && n >= 3)
		{
			double de, dn;

			if (! scan_double (items[1], &de))
				continue;
			if (! scan_double (items[2], &dn))
				continue;

			north = dn + .5;
			east  = de + .5;

			if (n == 3)
				desc = "";
			else
				desc = items[3];
			add_site (site_list, north, east, desc);
			continue;
		}

		/* allow "point" to be missing */
		if (n >= 2)
		{
			double de, dn;

			if (! scan_double (items[0], &de))
				continue;
			if (! scan_double (items[1], &dn))
				continue;

			north = dn + .5;
			east  = de + .5;

			if (n == 2)
				desc = "";
			else
				desc = items[2];
			add_site (site_list, north, east, desc);
			continue;
		}
	}
	rewind_site_list (site_list);
	return 1;
}
