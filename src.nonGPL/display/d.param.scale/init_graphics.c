/*****************************************************************************/
/***                                                                       ***/
/***                          init_graphics()                              ***/
/***   	   Initialises all graphics calles for interactive mouse query.	   ***/
/***               Jo Wood,Dept. of Geography, 4th December 1995           ***/
/***                                                                       ***/
/*****************************************************************************/

#include <stdlib.h>
#include "raster.h"
#include "display.h"
#include "D.h"
#include "param.h"

extern char* driver;
  
int 
init_graphics (void)
{

    /*------------------------------------------------------------------*/
    /*        			INITIALISE			 	*/
    /*------------------------------------------------------------------*/

    int   bot,top,lef,rit;		/* Window boundaries.		*/

    *dem_frame   = 0;			/* Initialise frame names.	*/
    *graph_frame = 0;


    /*------------------------------------------------------------------*/
    /*        OPEN GRAPHICS DRIVER AND CHECK WINDOW COORDINATES 	*/
    /*------------------------------------------------------------------*/

    system("d.frame -e; d.erase white");
    R_open_driver();

    D_get_screen_window(&top,&bot,&lef,&rit);	/* Get window coords.	*/

    D_new_window(graph_frame,bot/2 + 4, bot-1, lef+4, rit-4);


    D_new_window(dem_frame, top+1, bot/2, (rit-lef)/4, 3*(rit-lef)/4);
    D_set_cur_wind(dem_frame);
    Dcell(rast_in_name,mapset_in,0);

    return 0;
}
