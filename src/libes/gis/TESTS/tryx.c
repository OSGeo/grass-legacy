#include "gis.h"

main()
{
	char name[40], *mapset, *desc;
	double north, east;
	FILE *fd;
	G_gisinit ("");

	mapset = G_ask_sites_old ("", name);
	if (!mapset) exit(0);

	fd = G_fopen_sites_old (name, mapset);
	if (fd == NULL)
		G_fatal_error ("OOPS");
	while (G_get_site (fd, &east, &north, &desc) > 0)
		printf ("%lf %lf %s\n", east, north, desc);
}
