/* %W% %G% */

#include <stdio.h>
#include "gis.h"
#include "disfrm.h"

writecats(dist, name)
    int *dist ;
    char *name ;
{
    char buffer[10] ;
    struct Categories cats ;
    int i ;
    CELL n;

    G_init_cats ((CELL)0, "Pre-Proximity Analysis", &cats);
    G_set_cat ((CELL)0, "No distance", &cats) ;
    n = 0;
    for (i=1; i<MAXDIST; i++)
	if (dist[i])
	{
	    sprintf(buffer,"%d", dist[i]) ;
	    G_set_cat (++n, buffer, &cats) ;
	}

    G_write_cats(name, &cats) ;
    G_free_cats (&cats);
}
