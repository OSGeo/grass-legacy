#include "gis.h"

new_colors (name, reclass)
    char *name;
    struct Reclass *reclass;
{
    struct Colors old_colr, new_colr;
    int i;
    int red,grn,blu;
    CELL min, max, new_cat, old_cat;

    G_suppress_warnings (1);
    i = G_read_colors (reclass->name, reclass->mapset, &old_colr);
    G_suppress_warnings (0);
    if (i < 0) return;

    G_init_colors (&new_colr);

/* find min,max for new layer */
    min = max = 0;
    for (i = 0; i < reclass->num; i++)
    {
	if (reclass->table[i])
	{
	    if (min == 0)
		min = max = reclass->table[i];
	    else if (reclass->table[i] > max)
		max = reclass->table[i];
	    else if (reclass->table[i] < min)
		min = reclass->table[i];
	}
    }

    for (new_cat = min; new_cat <= max; new_cat++)
    {
	old_cat = old_colr.min;
	if (old_cat < reclass->min)
	    old_cat = reclass->min;
	for ( ; old_cat <= old_colr.max && old_cat <= reclass->max; old_cat++)
	{
	    if (reclass->table[old_cat-reclass->min] == new_cat)
	    {
		G_get_color (old_cat, &red, &grn, &blu, &old_colr);
		G_set_color (new_cat, red, grn, blu, &new_colr);
		break;
	    }
	}
    }
    G_get_color (0, &red, &grn, &blu, &old_colr);
    G_set_color (0, red, grn, blu, &new_colr);
    G_write_colors (name, G_mapset(), &new_colr);
}
