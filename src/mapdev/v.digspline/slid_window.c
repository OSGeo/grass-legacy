/*  @(#)slid_window.c    1.0  12/18/89  */
/*
**-->  Written by Ron Glenn  12/1989
**  US Dept. Agri., Soil COnservation Service
**   based upon mk_window.c,  by Dave Gerdes, CERL
*/

#include "digit.h"
#include "wind.h"

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


    Clear_info ();

    while (1)
    {
	_Clear_base ();
	_Write_base (12, "Buttons:") ;
	_Write_base (13, "Left:   Specify new window CENTER") ;
	_Write_base (14, "Middle: Abort/Quit");
	Write_base  (15, "Right:  Specify new window CENTER") ;

	R_get_location_with_pointer (&screen_x, &screen_y, &button);
	flush_keyboard (); /*ADDED*/
	Clear_info ();

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
		Clear_base ();
	        replot(CM);
		Clear_info();

		break;

	    case 2:
		return (0);
		break;


	default:
	        return(1) ;
	        break ;
	} /* end of switch */
    } /* end of while */
}
