#include "gis.h"
#include "local_proto.h"

int zoomwindow (int quiet, int rotate, double magnify, char pan)
{
    struct Cell_head window, oldwindow ;
    char *err;
    int quitonly;

    G_get_set_window(&window);
    G_copy((char *) &oldwindow, (char *) &window, sizeof(window));

    if (window.proj != PROJECTION_LL)
	rotate = 0;

    while(1)
    {
	if (rotate)
	    quitonly=make_window_center (&window, magnify, -1.0, -1.0);
	else
	    quitonly=make_window_box (&window, magnify, pan);

	/* quitonly = 0: Zoom
	 * quitonly = 1: Quit
	 * quitonly = 2: Unzoom
	 */
	if (quitonly == 1) 
	  break; /* no action was taken */
	else	  
	{
	  if(quitonly == 2 &&
	     U_east <= oldwindow.east && U_west >= oldwindow.west &&
	     U_south >= oldwindow.south && U_north <= oldwindow.north)
	  {
		break;
	  }
	  
	  if(window.east > U_east)
		  window.east = U_east;
	  if(window.west < U_west)
		  window.west = U_west;
	  if(window.south < U_south)
		  window.south = U_south;
	  if(window.north > U_north)
		  window.north = U_north;

	  if (err = G_adjust_Cell_head (&window, 0, 0))
	  {
	    	just_click(err);
	    	continue;
	  }

          G_put_window(&window);
          G_set_window(&window);
	  redraw();
	  
	  if (!quitonly)
	  {
	    if (yes("Accept new region?"))
	      break;

    	    G_copy((char *) &window, (char *) &oldwindow, sizeof(window));
            G_put_window(&window);
	    G_set_window(&window);
	    redraw();

	    if (!yes("Try again?"))
	      return 1;
	  }
	  else
	    break;
	}
    }

    if(!quiet)
    {
	fprintf(stderr, "This region now saved as current region.\n\n") ;
	fprintf(stderr, "Note: run 'd.erase' for the new region to affect the graphics.\n");
    }
    return(quitonly) ;
}
