#include <stdio.h>
#include "gis.h"
#include "site.h"
#include "proto.h"

/* update 12/99 */
/* 
 * updated 28 June 1995 to use new sites API.
 * Only uses floating point attributes. 
 * mccauley
 */

void read_sites ( char *name)
{
    char *mapset;
    FILE *fd;
    int n, c, i, d;
    Site *site;

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

    if (G_site_describe (fd, &n, &c, &i, &d)!=0)
      G_fatal_error("failed to guess format");

/*    site = G_site_new_struct (-1,2, 0, 1);*/ /* updated 12/99 MN */
    site = G_site_new_struct (c, n, i, d);

    fprintf (stderr, "Reading sites map (%s) ...", name);


    if (d==0)
    {
      fprintf(stderr,"\n");
      G_warning("I'm finding records that do not have a floating point attributes (fields prefixed with '%').");
    }

    while (G_site_get(fd, site) >= 0)
    {
	newpoint(site->dbl_att[0],site->east,site->north);
    }
    fclose (fd);
    fprintf (stderr, "\n");
}
