#include "gis.h"
#include "local_proto.h"

int zoomwindow (int quiet, int rotate, double magnify)
{
    struct Cell_head window, oldwindow ;
    char *err;
    int quitonly;

    G_get_set_window(&window) ;
    G_copy((char *) &oldwindow, (char *) &window, sizeof(window));

    if (window.proj != PROJECTION_LL)
	rotate = 0;

    while(1)
    {
	if (rotate)
	    quitonly=make_window_center (&window, magnify);
	else
	    quitonly=make_window_box (&window, magnify);

	if (quitonly == 1) 
	  break; /* no action was taken */
	else	  
	{
	  if (err = G_adjust_Cell_head (&window, 0, 0))
	  {
	    	just_click(err);
	    	continue;
	  }
          G_put_window(&window) ;
          G_set_window(&window) ;
	  redraw();
	  
	  if (!quitonly)
	  {
	    if (yes("Accept new region?"))
	      break;

            G_put_window(&oldwindow) ;
	    G_set_window(&oldwindow);
	    redraw();
	    if (!yes("Try again?"))
	      return 1;
	  }
	  else
	    break;
	}
    }

    /*
    if (!quitonly || quitonly == 2)
    {
       G_put_window(&window) ;
    }
    */

    if(!quiet)
    {
	fprintf(stderr, "This region now saved as current region.\n\n") ;
	fprintf(stderr, "Note: run 'd.erase' for the new region to affect the graphics.\n");
    }
    return(quitonly) ;
}
