#include "gis.h"

int 
read_sites (char *name)
{
    char *mapset;
    FILE *fd;
    double east, north;
    char *desc;
    double z;
    int count, errors;

    mapset = G_find_sites (name,"");
    if (mapset == NULL)
    {
	fprintf (stderr, "%s: %s - sites map not found\n", G_program_name(), name);
	exit(1);
    }
    fd = G_fopen_sites_old(name,mapset);
    if (fd == NULL)
    {
	fprintf (stderr, "%s: %s - can't open sites map\n", G_program_name(), name);
	exit(1);
    }

    fprintf (stderr, "Reading sites map <%s> ...", name);
    count = errors = 0;
    while (G_get_site(fd, &east, &north, &desc) > 0)
    {
	if (sscanf (desc, "%lf", &z) == 1 || sscanf (desc, "#%lf", &z) == 1)
	{
	    count++;
	    newpoint(z,east,north);
	}
	else
	    errors++;
    }
    fclose (fd);
    fprintf (stderr, "\n");

    if (errors)
    {
	fprintf (stderr, "Warning: %s - %sdid not contain %svalid elevation values\n",
		name, count?"some sites":"", count?"":"any");
    }
}
