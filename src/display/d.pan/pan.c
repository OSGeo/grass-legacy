#include "gis.h"
#include "local_proto.h"

int pan( int quiet, double mag)
{
    struct Cell_head window ;
    char *err;
    int save;

    G_get_window(&window) ;

    while(1)
    {
	make_window_center (&window,mag);
	if (err = G_adjust_Cell_head (&window, 0, 0))
	{
	    just_click(err);
	    continue;
	}
	if (yes("Accept new region?"))
	    break;
	if (!yes("Try again?"))
	    return 1;
    }

    G_put_window(&window) ;
    if(!quiet)
    {
	fprintf(stderr, "This region now saved as current region.\n\n") ;
	fprintf(stderr, "Note: run 'd.erase' for the new region to affect the graphics.\n");
    }
    return(0) ;
}
