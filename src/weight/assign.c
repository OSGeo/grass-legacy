#include "include.h"

assign_single(name, cat, val)
    char *name ;
    long cat ;
    long val ;
{
    int map ;

    for(map=0; map<MAX_MAPS; map++)
    {
	if (!strncmp(name, mapdef[map].name, 14))
	{
	    G_set_histogram ((CELL) cat, val, &mapdef[map].histo);
	    G_sort_histogram (&mapdef[map].histo);
	    printf("    category %ld in map %s assigned weight: %ld\n",
			cat, mapdef[map].name, val) ;
	    return 0;
	}
    }

    printf("    error: %s not a current mapname\n", name) ;
    return(-1) ;
}


assign_mult(name, cat1, cat2, val)
    char *name ;
    long cat1, cat2;
    long val ;
{
    int map ;
    long cat ;
    long tmp ;

    if (cat2 < cat1)
    {
	tmp = cat2 ;
	cat2 = cat1 ;
	cat1 = tmp ;
    }

    for(map=0; map<MAX_MAPS; map++)
    {
	if (!strncmp(name, mapdef[map].name, 14))
	{
	    for (cat=cat1; cat<=cat2; cat++)
		G_set_histogram ((CELL)cat, val, &mapdef[map].histo);
	    printf("    categories %ld to %ld in map %s assigned weight: %ld\n",
			cat1, cat2, mapdef[map].name, val) ;
	    G_sort_histogram (&mapdef[map].histo);
	    return 0;
	}
    }

    printf("    error: %s not a current mapname\n", name) ;
    return(-1) ;
}
