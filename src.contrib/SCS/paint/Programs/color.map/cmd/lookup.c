#include "gis.h"
#include "parms.h"
#include "misc.h"

/* if cat has a pattern imposed on it, return the color which
   corresponds to the row,col for the pattern for cat,
   else return -1
*/
lookup_from_pattern (cat, row, col)
    register int row, col;
    register CELL cat;
{
    register PATTERN *p;

    if (parms.pattern == NULL)
	return -1;

    if ((cat = cat_color_num ((int)cat)) < 0)
	return WHITE;

    if ((p = parms.pattern[cat]) == (PATTERN *) NULL)
	return -1;

    return p->colors[p->pat[row%p->nrows][col%p->ncols]];
}
