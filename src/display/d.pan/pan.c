#include "gis.h"
#include "local_proto.h"

int pan( int quiet, double mag)
{
    struct Cell_head window, oldwindow ;
    char *err;
    int save;

    G_get_set_window(&window) ;
    G_copy((char *) &oldwindow, (char *) &window, sizeof(window));

    while(1)
    {
	if(make_window_center (&window,mag))
	      return 0;

	if(!(window.east <= U_west || window.west >= U_east ||
	   window.south >= U_north || window.north <= U_south))
	{
	  if (err = G_adjust_Cell_head (&window, 0, 0))
	  {
	      just_click(err);
	      continue;
	  }

	  G_put_window(&window);
	  G_set_window(&window);
	  redraw();

	  if (yes("Accept new region?"))
	      continue;

	  G_copy((char *) &window, (char *) &oldwindow, sizeof(window));
	  G_put_window(&window);
	  G_set_window(&window);
	  redraw();
	}
	else
	{
	  fprintf(stderr, "** Reached at region boundary **\n");
	}
    }

#ifdef QUIET
    if(!quiet)
    {
	fprintf(stderr, "This region now saved as current region.\n\n") ;
	fprintf(stderr, "Note: run 'd.erase' for the new region to affect the graphics.\n");
    }
#endif
    return(0) ;
}
