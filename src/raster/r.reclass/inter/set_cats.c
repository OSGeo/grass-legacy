#include "gis.h"

set_new_cats(cats, new_cats, table, min, max) 
    struct Categories *cats ;
    struct Categories *new_cats ;
    long *table ;
    CELL min, max;
{
    long i,j ;
    CELL cMin = table[0];
    CELL cMax = cMin;

    for (i=(long)min+1; i <= (long)max; i++) {
	if (cMin > table[i-min])
	    cMin = table[i-min];
	if (cMax < table[i-min])
	    cMax = table[i-min];
	}

    for (j=(long)cMin; j <= (long)cMax;j++)
         for (i=(long)min; i <= (long)max; i++)
        {
	if (table[i-min] == j)
            G_set_cat (j, G_get_cat ((CELL)i, cats), new_cats);
        }
}
