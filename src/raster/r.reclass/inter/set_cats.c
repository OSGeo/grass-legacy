#include "gis.h"
/* returns max category values or null if there are no non-null values */

CELL set_new_cats(
    struct Categories *cats,
    struct Categories *new_cats,
    CELL *table ,
    CELL min,CELL max)
{
    long i,j ;
    CELL cMin;
    CELL cMax;

    /* look for the first non-null values in the table */
    for (j=(long)min; j <= (long)max; j++)
    {
	if (!G_is_c_null_value(&table[j-min]))
	     break;
    }
    if (j >= (long)max) return table[0];
    /* no non-null values */

    /* compute table min and max values */
    cMax = cMin = table[j - min];
    for (i=j; i <= (long)max; i++) {
	if (!G_is_c_null_value(&table[i-min]) && cMin > table[i-min])
	    cMin = table[i-min];
	if (!G_is_c_null_value(&table[i-min]) && cMax < table[i-min])
	    cMax = table[i-min];
	}

    for (j=(long)cMin; j <= (long)cMax;j++)
         for (i=(long)min; i <= (long)max; i++)
        {
	if (table[i-min] == j)
            G_set_cat (j, G_get_cat ((CELL)i, cats), new_cats);
        }
    return cMax;
}
