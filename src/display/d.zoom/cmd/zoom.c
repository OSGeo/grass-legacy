#include "gis.h"
#include "local_proto.h"

int zoomwindow (int quiet, int rotate, double magnify)
{
    struct Cell_head window ;
    char *err;
    int quitonly;

    G_get_window(&window) ;
    if (window.proj != PROJECTION_LL)
	rotate = 0;

    while(1)
    {
	if (rotate)
	    quitonly=make_window_center (&window, magnify);
	else
	    quitonly=make_window_box (&window, magnify);

	if (quitonly) 
	  break; /* no action was taken */
	else	  
	{
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
    }

    if (!quitonly || quitonly == 2)
       G_put_window(&window) ;

    if(!quiet)
    {
	fprintf(stderr, "This region now saved as current region.\n\n") ;
	fprintf(stderr, "Note: run 'd.erase' for the new region to affect the graphics.\n");
    }
    return(0) ;
}
