#include "gis.h"
#include "parms.h"
#include "misc.h"


set_pattern (cat, name)
    char *name;
{
    PATTERN p;
    int n;

/* translate cat into color lookup number */
    if ((n = cat_color_num (cat)) < 0) return 1;

    parms.pattern[n] = NULL;

    if (!get_pattern (name, &p))
	return 0;

    parms.pattern[n] = (PATTERN *) G_malloc (sizeof (PATTERN));
    G_copy (parms.pattern[n] , &p, sizeof (PATTERN));
    return 1;
}

set_all_patterns ()
{
    PATTERN p;
    int cat;
    int n;
    int first;


    first = 1;
    for (cat = parms.pcolr.min; cat <= parms.pcolr.max; cat++)
    {
	if (!next_pattern (&p, first)) break;
	n = cat_color_num(cat);
	if (n < 0) continue;

	first = 0;
	parms.pattern[n] = (PATTERN *) G_malloc (sizeof (PATTERN));
	G_copy (parms.pattern[n] , &p, sizeof (PATTERN));
    }
}
