#include <grass/gis.h>
#include "options.h"

int normal(char *mapset,
	struct Cell_head *window,
	struct Categories *cats)
{

/* Print the infor to stdout */
	fprintf (stdout, ".C %s\n", color) ;
	fprintf (stdout, ".S %f\n", size) ;
	fprintf (stdout, "LOCATION: %s\n", G_location()) ;
	fprintf (stdout, "%s in %s\n", map_name, mapset) ;
	fprintf (stdout, "%s\n", cats->title) ;
	fprintf (stdout, "North: %10.2f  South: %10.2f\n",
		window->north, window->south) ;
	fprintf (stdout, "West:  %10.2f  East:  %10.2f\n",
		window->west, window->east) ;
	fprintf (stdout, "Resolution: n-s: %7.2f  e-w: %7.2f\n",
		window->ns_res, window->ns_res) ;

	return 0;
}
