/**************************************************************
 * 
 * char *G_get_cell_title (name, mapset)
 *   char *name        name of map file
 *   char *mapset      mapset containing name
 *
 *   returns pointer to string containing cell title. (from cats file)
 *************************************************************/

#include "gis.h"
#include "site.h"

char *
G_get_sites_title (name, mapset)
    char *name;
    char *mapset;
{
    FILE *fd;
    int stat;
    Site_head head;

    stat = -1;
    fd = G_fopen_old ("site_lists", name, mapset);
    if (fd)
    {
	stat = 1;
	if (G_site_get_head (fd, &head) != 0)
	    stat = -1;

	fclose (fd);
    }

    if (stat < 0)
	return G_store("");

    if (head.name)   G_free(head.name);
    if (head.form)   G_free(head.form);
    if (head.labels) G_free(head.labels);
    if (head.stime)  G_free(head.stime);
    if (head.time)   G_free(head.time);

    if (!head.desc)
	return G_store("");

    G_strip (head.desc);

    return head.desc;
}
