#include "globals.h"

edge_point (x, y)
	register int x, y;
{
	register int n;

	n = Region.perimeter_npoints++;
	Region.perimeter[n].x = x;
	Region.perimeter[n].y = y;
}
