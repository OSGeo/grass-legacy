
#include "gis.h"
/*
 * patch in non-zero data over zero data
 * keep track of the categories which are patched in
 * for later use in constructing the new category and color files
 *
 * returns: 1 the result still contains zeros
 *          0 the result contains no zero values
 */

do_patch (result, patch, statf, ncols)
    CELL *result, *patch;
    struct Cell_stats *statf;
{
    int more;

    more = 0;
    while (ncols-- > 0)
    {
	if (*result == 0)
	{
	    if(*result = *patch)
		G_update_cell_stats (result, 1, statf);
	    else
		more = 1;
	}
	result++;
	patch++;
    }
    return more;
}
