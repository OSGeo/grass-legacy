#include "gis.h"

/* =========================================================================
 * write_supp generates cats and colr support files
 * for named and overlay nodes
 */

write_supp(name, mapset)
    char *name ;
    char *mapset ;
{
    struct Colors color ;
    struct Categories cats ;
    struct Range range;

/* make the categories support file */
    if(G_read_cats(name, mapset, &cats) != -1)
    {
	if(G_edit_cats(name, &cats, -1) != -1)
	    G_write_cats (name, &cats);
	G_free_cats (&cats);
    }

/* make the color support file */
    G_read_range (name, mapset, &range);
    make_colors(&color,
	range.nmin?range.nmin:range.pmin,
	range.pmax?range.pmax:range.nmax);
    G_write_colors(name, mapset, &color) ;
    G_free_colors (&color);

    return(1) ;
}
