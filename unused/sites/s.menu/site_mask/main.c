#include <string.h>
#include "gis.h"
#include "site.h"
#include "local_proto.h"

int main (int argc, char *argv[])
{
	SITE_LIST sites_new, sites_old ;
	char layer[100];
	char *mapset;
	char *mask;
	struct Categories cats;
	int cellfd;
	int len;

	G_gisinit (argv[0]);
	new_mask_screen();
	initialize_site_list (&sites_old);
	if (!get_site_list (&sites_old, argv[1]))
		exit (-1);

	mapset = G_ask_cell_old ("select a mask layer", layer);
	if (!mapset)
		exit(0);
	if ((cellfd = G_open_cell_old (layer, mapset)) < 0)
	{
		fprintf (stdout,"<%s> can't open raster file\n", layer);
		exit(-1);
	}
	if (G_read_cats (layer, mapset, &cats) < 0)
		exit(-1);

/* get table length.
 * must fit into an int since malloc requires int arguments
 */
	len = cats.num+1;
	if (len != (cats.num+1))
	{
		fprintf (stdout,"Too many categories\n");
		exit(-1);
	}
	mask = G_malloc (len);
	if (!get_mask (layer, mapset, mask, &cats))
		exit(-1);

	rewind_site_list (&sites_old);
	initialize_site_list (&sites_new);
	strcpy (sites_new.desc, sites_old.desc);

	if (!mask_sites (&sites_old, &sites_new, cellfd, mask, cats.num))
		exit(-1);

	if (!put_site_list (&sites_new, argv[1], 0, 0))
		exit(-1);

	exit(0);
}
