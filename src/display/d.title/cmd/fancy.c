#include "gis.h"
#include "options.h"

fancy(mapset, window, cats)
	char *mapset ;
	struct Cell_head *window ;
	struct Categories *cats ;
{

/* Print the infor to stdout */
	printf( ".C %s\n", "green") ;
	printf( ".S %f\n", size + 1.0) ;
	printf( "LOCATION: %s\n", G_location()) ;
	printf( ".C %s\n", color) ;
	printf( ".S %f\n", size) ;
	printf( "%s in %s\n", map_name, mapset) ;
	printf( "%s\n", cats->title) ;
	printf( "North: %10.2lf  South: %10.2lf\n",
		window->north, window->south) ;
	printf( "West:  %10.2lf  East:  %10.2lf\n",
		window->west, window->east) ;
	printf( "Resolution: n-s: %7.2lf  e-w: %7.2lf\n",
		window->ns_res, window->ns_res) ;
}
