/* %W% %G% */

#include "gis.h"
#include "stdio.h"

update_cats(res, cats, ncats, dist)
    double res ;
    struct Categories *cats ;
    int dist[] ;
{
    int i ;
    char buffer[80] ;

    G_init_cats (0, "Proximity analysis", cats) ;

    G_set_cat (0, "Locations from which distances calculated", cats) ;

    sprintf(buffer,"%.2f to %.2f meters", 0.0, res * dist[0] ) ;
    G_set_cat (1, buffer, cats) ;

    for(i=2; i<ncats; i++)
    {
	sprintf(buffer,"%.2f to %.2f meters", res*dist[i-2], res*dist[i-1]) ;
	G_set_cat (i, buffer, cats) ;
    }

    sprintf(buffer,"Greater than %.2f meters",
	    res * dist[i - 2] ) ;
    G_set_cat (i, buffer, cats) ;
}
