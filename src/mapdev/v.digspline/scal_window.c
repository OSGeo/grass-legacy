/*  @(#)scal_window.c    1.0  01/28/90  */
/*
**-->  Written by R.L.Glenn, USDA, SCS
**  from mk_window.c, By Dave Gerdes
**  US Army Construction Engineering Research Lab
*/

#include "digit.h"
#include "wind.h"

#define SCALE_FACTOR 0.8

scal_window_w_mouse ()
{
    int screen_x, screen_y ;
    int button, yn ;
    double N, S, E, W;
    double delta;
    char buffer[64] ;

    Clear_info ();

top:

    while (1)
    {
	_Clear_base ();
	_Write_base (12, "Buttons:") ;
	_Write_base (13, "Left:   Zoom in") ;
	_Write_base (14, "Middle: Abort/Quit") ;
	Write_base  (15, "Right:  Zoom out") ;

        screen_x = screen_y = 1;
	R_get_location_with_pointer(&screen_x, &screen_y, &button) ;
	flush_keyboard (); /*ADDED*/
	Clear_info ();

	switch (button)
        {
	    case 1:
		/* ZOOM IN */
                W = U_west  + (U_east - U_west)   * (1. - SCALE_FACTOR);
                E = U_east  - (U_east - U_west)   * (1. - SCALE_FACTOR);
                S = U_south + (U_north - U_south) * (1. - SCALE_FACTOR);
                N = U_north - (U_north - U_south) * (1. - SCALE_FACTOR);

		Clear_base ();
	        window_rout (N, S, E, W);
                clear_window ();
                replot(CM); 
		Clear_info ();
		break;

	    case 2:
                return(0);
		break;

	    case 3:
		/* ZOOM OUT */
                W = U_west  - (U_east - U_west)   * (1. - SCALE_FACTOR);
                E = U_east  + (U_east - U_west)   * (1. - SCALE_FACTOR);
                S = U_south - (U_north - U_south) * (1. - SCALE_FACTOR);
                N = U_north + (U_north - U_south) * (1. - SCALE_FACTOR);

		Clear_base ();
	        window_rout (N, S, E, W);
                clear_window ();
                replot(CM);
		Clear_info ();

	        break ;

	    default:
	        return(1) ;
	        break ;

	 } /* end switch */

      } /* end while */
}
