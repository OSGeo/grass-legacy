#include "gis.h"
/* For lat-lon,
 *   this routine adjusts the values of the longitudes so that
 *   the difference between them is less than 180.
 *   It does NOT change their true position.
 * For other databases
 *   this routine does nothing
 */

/*!
 * \brief shortest way between eastings
 *
 * If the database projection is PROJECTION_LL, then
 * <b>east1,east2</b> are changed so that they are no more than 180 degrees
 * apart. Their true locations are not changed. If the database projection is not
 * PROJECTION_LL, then <b>east1,east2</b> are not changed.
 *
 *  \param east1
 *  \param east2
 *  \return int
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
