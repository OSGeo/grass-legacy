#include "glob.h"

disconnect (pt1, pt2)
    int pt1, pt2;
{
    doit (pt1, pt2);
    doit (pt2, pt1);
}

static
doit (pt1, pt2)
    int pt1, pt2;
{
    int n, count;

    if (pt1 < 0) return;

    count = get_number_of_neighbors (pt1);
    for (n = 0; n < count; n++)
    {
	if (get_neighbor (pt1, n) == pt2)
	    unset_neighbor (pt1, n);
    }
}
