#include "gis.h"
#include "parms.h"
#include "misc.h"
#include "local_proto.h"

/* if cat has a pattern imposed on it, return the color which
   corresponds to the row,col for the pattern for cat,
   else return -1
*/
int lookup_from_pattern (register CELL cat, register int row, register int col)
{
    register PATTERN *p;

    if (parms.pattern == NULL)
	return -1;

    if ((cat = cat_color_num ((int)cat)) < 0)
	return WHITE;

    if ((p = parms.pattern[cat]) == (PATTERN *) NULL)
	return -1;

    return p->colors[(int)p->pat[row%p->nrows][col%p->ncols]];
}
