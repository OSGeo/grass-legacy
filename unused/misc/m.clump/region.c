#include "glob.h"

static int set = 0;
static struct Cell_head region;

void
set_region()
{
    G_get_set_window (&region);
    set = 1;
}

have_region()
{
    return set;
}

int
point_in_region (x, y)
    double x, y;
{
    if (!have_region())
	return 1;

    x = G_adjust_easting (x, &region); /* for lat/lon */
    if (x < region.west) return 0;
    if (x > region.east) return 0;
    if (y < region.south) return 0;
    if (y > region.north) return 0;

    return 1;
}
