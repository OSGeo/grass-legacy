#include "gis.h"
#include "site.h"

int count_sites (SITE_LIST *site_list,int clipped)
{
	int count;
	struct Cell_head window;
	double north;
	double east;
	char *desc;

	if (clipped)
		if (G_get_window (&window) == -1)
			clipped = 0;

	rewind_site_list (site_list);

	count = 0;
	while (next_site (site_list, &north, &east, &desc))
		if (!clipped || within_window (north, east, &window))
			count++;

	return count;
}
