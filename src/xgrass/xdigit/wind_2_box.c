/*******************************************************************
 *
 *   G_adjust_window_to_box (src, dst, rows, cols)
 *
 *     creates a new window (dst) from a window (src)
 *     which fits into the rectangular box with dimensions
 *     rows by cols
 *
 *   parms:
 *      struct Cell_head *src
 *      struct Cell_head *dst
 *      int rows
 *      int cols
 *
 *   returns:
 *      nothing
 **********************************************************************/

#include "gis.h"


G_adjust_window_to_box (src, dst, rows, cols)
    struct Cell_head *src, *dst;
{
    double ew, ns;

    G_copy (dst, src, sizeof(*dst));

/*DEBUG*/ 
	debugf ("ADJUST  rows %d  cols %d\n", rows, cols);
	debugf ("    ns_res  %lf   ew_res %lf\n", src->ns_res, src->ew_res);
	debugf ("    rows    %d    cols   %d\n", src->rows, src->cols);
/* calculate the effective resolutions */
    ns = (src->ns_res * src->rows) / rows;
    ew = (src->ew_res * src->cols) / cols;

/* set both resolutions equal to the larger */
/*DEBUG*/ debugf ("ADJUST: NS = %lf  EW = %lf\n", ns, ew);
    if (ns > ew)
	ew = ns;
    else
	ns = ew;

    dst->ns_res = ns;
    dst->ew_res = ew;

/* compute rows and cols */
    dst->rows = (dst->north - dst->south) / dst->ns_res;
    dst->cols = (dst->east  - dst->west ) / dst->ew_res;
}
