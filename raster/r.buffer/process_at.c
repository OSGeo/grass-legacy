#include "distance.h"

int process_at (int from_row, int to_row, int start_col, int first_zone)
{
    register MAPTYPE *to_ptr, *from_ptr;
    register int col, cur_zone;

	/* find all adjacent 1 cells 
	 * stop at last 1 in the from_row
	 * return position of last 1
	 */

    col = start_col;
    from_ptr = map + MAPINDEX(from_row, col);
    to_ptr = map + MAPINDEX(to_row, col);
    while (col <= maxcol && *from_ptr == 1)
    {
	if(cur_zone = *to_ptr)
	    cur_zone -= ZONE_INCR;
	else
	    cur_zone = ndist;

	if (first_zone < cur_zone)
	    *to_ptr = first_zone + ZONE_INCR;

	to_ptr++;
	col++;
	from_ptr++;
    }
    return col-1;
}
