#include "gis.h"

set_new_cats(cats, new_cats, table) 
    struct Categories *cats ;
    struct Categories *new_cats ;
    long *table ;
{
    long i ;
    long j ;

    for (i=0; i <= new_cats->num; i++)
    {
	for (j=0; j <= cats->num; j++)
	{
	    if (table[j] == i)
	    {
		G_set_cat ((CELL)i, G_get_cat ((CELL)j, cats), new_cats);
		break ;
	    }
	}
    }
}
