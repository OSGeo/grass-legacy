/*************************************************************************
 *   char *
 *   G_ask_sites_new(prompt, name)) 
 *       asks user to input name of a new site list file
 *
 *   char *
 *   G_ask_sites_old(prompt, name) 
 *       asks user to input name of an existing site list file
 *
 *   char *
 *   G_ask_sites_any(prompt, name)
 *       asks user to input any site list name
 *
 *   char *
 *   G_ask_sites_in_mapset(prompt, name)
 *       asks user to input name of an existing site list file in
 *       current mapset
 *
 *   parms:
 *      char *prompt    optional prompt for user
 *      char *name      buffer to hold name of map found
 *
 *   returns:
 *      char *pointer to a string with name of mapset
 *       where file was found, or NULL if not found
 *
 *   note:
 *      rejects all names that begin with .
 **********************************************************************
 *
 *  FILE *
 *  G_fopen_sites_old (name, mapset)
 *      opens the existing site list file 'name' in the 'mapset'
 *
 *  FILE *
 *  G_fopen_sites_new (name)
 *      opens a new site list file 'name' in the current mapset
 *
 *  parms
 *      char *name            map file name
 *      char *mapset          mapset containing map "name"
 *
 **********************************************************************
 *
 * G_get_site (fd, east, north, desc)
 *    FILE *fd;
 *    double *east, *north;
 *    char **desc;
 *
 *
 *    Reads the next "point" from the site file open on fd. The
 *    east and north are set, and desc is set to point to the
 *    description of the site. (desc points to static memory
 *    so each call overrides the previous
 *
 *    Returns: 1 got a site
 *            -1 no more sites
 *
 * G_put_site (fd, east, north, desc)
 *    FILE *fd;
 *    double east, north;
 *    char *desc;
 *
 *    Writes a site (east, north, desc) to file open on fd.
 **********************************************************************/

#include "gis.h"

char *
G_ask_sites_new (prompt,name)
    char *prompt;
    char *name;
{
    char *G_ask_new();

    return G_ask_new (prompt, name, "site_lists", "site list");
}

char *
G_ask_sites_old (prompt,name)
    char *prompt;
    char *name;
{
    char *G_ask_old();

    return G_ask_old (prompt, name, "site_lists", "site list");
}

char *
G_ask_sites_any (prompt,name)
    char *prompt;
    char *name;
{
    char *G_ask_any();

    return G_ask_any (prompt, name, "site_lists", "site list", 1);
}

char *
G_ask_sites_in_mapset (prompt,name)
    char *prompt;
    char *name;
{
    char *G_ask_in_mapset();

    return G_ask_in_mapset (prompt, name, "site_lists", "site list");
}

FILE *
G_fopen_sites_old (name, mapset)
    char *name;
    char *mapset;
{
    return G_fopen_old ("site_lists", name, mapset);
}

FILE *
G_fopen_sites_new (name)
    char *name;
{
    return G_fopen_new ("site_lists", name);
}

G_get_site (fd, east, north, desc)
    FILE *fd;
    double *east, *north;
    char **desc;
{
    char buf[400];
    char temp[400];
    char ebuf[128], nbuf[128];
    static char *desc_ptr = NULL;

    if (desc_ptr != NULL)
    {
	free (desc_ptr);
	desc_ptr = NULL;
    }
    *temp = 0;
    while (fgets (buf, sizeof buf, fd))
    {
	if (sscanf (buf, "point|%[^|]|%[^|]|%[^\n]", ebuf, nbuf, temp) >= 2
	||  sscanf (buf, "%[^|]|%[^|]|%[^\n]", ebuf, nbuf, temp) >= 2)
	{
	    if (G_scan_northing (nbuf, north, G_projection())
	    &&  G_scan_easting (ebuf, east, G_projection()))
	    {
		G_strip (temp);
		*desc = desc_ptr = G_store (temp);
		return 1;
	    }
	}
    }
    return -1;
}

G_put_site (fd, east, north, desc)
    FILE *fd;
    double east, north;
    char *desc;
{
    char ebuf[128], nbuf[128];
    int fmt;

/*  fmt = G_projection(); */ 
    fmt = -1;

    G_format_northing (north, nbuf, fmt);
    G_format_easting  (east,  ebuf, fmt);
    fprintf (fd, "%s|%s|%s\n", ebuf, nbuf, desc?desc:"");
}
