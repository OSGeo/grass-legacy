/*  @(#)scal_window.c    1.0  01/28/90  */
/*
**-->  Written by R.L.Glenn, USDA, SCS
**  from mk_window.c, By Dave Gerdes
**  US Army Construction Engineering Research Lab
*/
/* modified by RL Glenn  12/1991
** USDA, SCS, Tech. Infor. Sys. Division
*/


#include "digit.h"
#include "wind.h"
#include "popup.h"

#define SCALE_FACTOR 0.8

scal_window_w_mouse ()
{
    int screen_x, screen_y ;
    int button, yn ;
    double N, S, E, W;
    double delta;
    char buffer[64] ;
    int menu_left, menu_top;
    int ret, chr, first=1;

    menu_left = Next_l + 1;
    menu_top = Next_t;
    buttons[0] = " ";
    buttons[1] = "Buttons:\0";
    buttons[2] = "Left:   Zoom in";
    buttons[3] = "Middle: Abort/Quit";
    buttons[4] = "Right:  Zoom out";
    buttons[5] = "  ";
    buttons[6] = '\0';

top:

    while (1)
    {
        Dchoose(MEN.name) ;
        popup_butns( menu_top, menu_left, buttons, "scal_wind", first) ;
	first = 0;
        Dchoose(DIG.name) ;

        screen_x = screen_y = 1;
	R_get_location_with_pointer(&screen_x, &screen_y, &button) ;
	flush_keyboard (); /*ADDED*/

	switch (button)
        {
	    case 1:
		/* ZOOM IN */
                W = U_west  + (U_east - U_west)   * (1. - SCALE_FACTOR);
                E = U_east  - (U_east - U_west)   * (1. - SCALE_FACTOR);
                S = U_south + (U_north - U_south) * (1. - SCALE_FACTOR);
                N = U_north - (U_north - U_south) * (1. - SCALE_FACTOR);

	        window_rout (N, S, E, W);
                clear_window ();
                replot(CM); 
		break;

	    case 2:
                erase_popup("scal_wind");
                return(0);
		break;

	    case 3:
		/* ZOOM OUT */
                W = U_west  - (U_east - U_west)   * (1. - SCALE_FACTOR);
                E = U_east  + (U_east - U_west)   * (1. - SCALE_FACTOR);
                S = U_south - (U_north - U_south) * (1. - SCALE_FACTOR);
                N = U_north + (U_north - U_south) * (1. - SCALE_FACTOR);

	        window_rout (N, S, E, W);
                clear_window ();
                replot(CM);

	        break ;

	    default:
                erase_popup("scal_wind");
	        return(1) ;
	        break ;

	 } /* end switch */

      } /* end while */
}
