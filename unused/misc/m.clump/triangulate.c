#include "glob.h"

void
triangulate_point_list()
{
    if (!be_quiet())
	fprintf (stderr, "Performing triangulation ...\n");
    makeNeighbors (pointlist.x, pointlist.y, pointlist.npoints, &pointlist.neighbors);
}
