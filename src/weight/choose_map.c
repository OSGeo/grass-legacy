#include "include.h"
#include "yes_no.h"

choose_map(name)
    char *name ;
{
    char *mapset ;
    int map ;
    int n;
    CELL min_cat, max_cat;

    for(map=0; map<MAX_MAPS; map++)
    {
	if (mapdef[map].used)
	    if (!strncmp(name, mapdef[map].name, 14))
	    {
		printf("    error: map <%s> already chosen\n", name) ;
		return(-1) ;
	    }
    }

    mapset = G_find_cell(name, "") ;
    if (mapset == 0)
    {
	printf("    error: map <%s> not found\n", name) ;
	return(-1) ;
    }

    for(map=0; map<MAX_MAPS; map++)
    {
	if (! mapdef[map].used)
	{
	    strncpy(mapdef[map].name, name, 14) ;
	    strncpy(mapdef[map].mapset, mapset, 14) ;
	    mapdef[map].used = YES ;
	    get_histo (name, mapset, &mapdef[map].histo);
	    G_zero_histogram (&mapdef[map].histo);
	    n = G_get_histogram_num (&mapdef[map].histo);
	    min_cat = G_get_histogram_cat (0, &mapdef[map].histo);
	    max_cat = G_get_histogram_cat (n-1, &mapdef[map].histo);

	    printf("   You have chosen map (%s) with category range %ld thru %ld\n",
		    mapdef[map].name, (long)min_cat, (long)max_cat);
	    return(0) ;
	}
    }

    printf("    error: Can't set any more maps\n") ;
    return(-1) ;
}

unchoose_map(name)
    char *name ;
{
    char *mapset ;
    int map ;

    for(map=0; map<MAX_MAPS; map++)
    {
	if (mapdef[map].used)
	    if (!strncmp(name, mapdef[map].name, 14))
	    {
		release_map (map);
		printf("   You have removed map : %s\n", name) ;
		return(0) ;
	    }
    }

    printf("    error: %s not already chosen\n", name) ;
    return(-1) ;
}

release_map (map)
{
    strcpy(mapdef[map].name, "") ;
    strcpy(mapdef[map].mapset, "") ;
    mapdef[map].used = NO ;
    G_free_histogram( &mapdef[map].histo);
}
