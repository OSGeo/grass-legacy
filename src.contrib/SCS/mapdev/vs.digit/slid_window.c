/*  @(#)slid_window.c    1.0  12/18/89  */
/*
**-->  Written by Ron Glenn  12/1989
**  US Dept. Agri., Soil COnservation Service
**   based upon mk_window.c,  by Dave Gerdes, CERL
*/
/* modified by RL Glenn  12/1991
** USDA, SCS, Tech. Infor. Sys. Division
*/


#include "digit.h"
#include "wind.h"
#include "popup.h"

slid_window_w_mouse ()
{
    char buffer[64] ;
    int screen_x, screen_y ;
    double ux1, uy1 ;
    double ux2, uy2 ;
    int button ;
    double N, S, E, W;
    int yn;
    double tmp;
    int menu_left, menu_top;
    int ret, chr, first=1;

    menu_left = Next_l + 1;
    menu_top = Next_t;

    buttons[0] = " ";
    buttons[1] = "Buttons:\0";
    buttons[2] = "Left:   Specify new window CENTER .";
    buttons[3] = "Middle: Abort/Quit";
    buttons[4] = "Right:  Specify new window CENTER .";
    buttons[5] = "  ";
    buttons[6] = '\0';

    while (1)
    {
        Dchoose(MEN.name) ;
        popup_butns( menu_top, menu_left, buttons, "slid_wind", first) ;
	first = 0;
        Dchoose(DIG.name) ;

	R_get_location_with_pointer (&screen_x, &screen_y, &button);
	flush_keyboard (); /*ADDED*/

	switch (button)
	{
	    case 1:
	    case 3:
		screen_to_utm ( screen_x, screen_y, &ux1, &uy1) ;
		tmp =  (ux1 - ((U_east + U_west) / 2));
		W = U_west + tmp;
		E = U_east + tmp;

		tmp =  (uy1 - ((U_north + U_south) / 2));
		S = U_south + tmp;
		N = U_north + tmp;

                clear_window ();
	        window_rout (N, S, E, W);
	        replot(CM);

		break;

	    case 2:
                erase_popup("slid_wind");
		return (0);
		break;


	default:
                erase_popup("slid_wind");
	        return(1) ;
	        break ;
	} /* end of switch */
    } /* end of while */
}
