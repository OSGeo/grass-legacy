
#include "gis.h"

window_work()
{
    struct Cell_head window ;
    char *err, *G_adjust_Cell_head();

    G_get_window(&window) ;

/* Give them a menu of options */
    do
    {
	make_window(&window) ;
	if (err = G_adjust_Cell_head (&window, 0, 0))
	    fprintf (stderr, "** %s **\r", err);
    } while (err);

    if (G_yes("Do you want to set your current map window to this? ", -1))
    {
	G_put_window(&window) ;
	fprintf(stderr, "This window now saved as current window.\n\n") ;
	fprintf(stderr, "Note: run 'Derase' for the new window to affect the graphics.\n");
	return(0) ;
    }
    else
    {
	fprintf(stderr, "This window NOT saved as current window.\n") ;
	return(1) ;
    }
}
