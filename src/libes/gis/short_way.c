#include "gis.h"
/* For lat-lon,
 *   this routine adjusts the values of the longitudes so that
 *   the difference between them is less than 180.
 *   It does NOT change their true position.
 * For other databases
 *   this routine does nothing
 */
int G_shortest_way (double *east1,double *east2)
{
    if (G_projection() == PROJECTION_LL)
    {
	if (*east1 > *east2)
	    while ((*east1-*east2) > 180)
		*east2 += 360;
	else if (*east2 > *east1)
	    while ((*east2-*east1) > 180)
		*east1 += 360;
    }

    return 0;
}
