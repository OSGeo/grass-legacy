/*  @(#)slid_window.c    1.0  12/18/89  */
/*
**-->  Written by Ron Glenn  12/1989
**  US Dept. Agri., Soil COnservation Service
**   based upon mk_window.c,  by Dave Gerdes, CERL
*/

#include "digit.h"
#include "raster.h"
#include "wind.h"
#include "keyboard.h"
#include "Map_proto.h"
#include "dig_curses.h"
#include "local_proto.h"

int 
slid_window_w_mouse (unsigned char type, struct line_pnts *Xpoints)
{
    char buffer[64] ;
    int screen_x, screen_y ;
    double ux1, uy1 ;
    double ux2, uy2 ;
    int button ;
    double N, S, E, W;
    int yn;
    double tmp1, tmp2, tmp3, tmp4;


    Clear_info ();

    while (1)
    {
	_Clear_base ();
	_Write_base (12, "Buttons:") ;
	_Write_base (13, "   Left:   Specify new window CENTER") ;
#ifdef ANOTHER_BUTTON
	_Write_base (14, "   Middle: Abort/Quit");
	Write_base  (15, "   Right:  Specify new window CENTER") ;
#else
	_Write_base (14, "   Middle: Specify new window CENTER");
	Write_base  (15, "   Right:  Abort/Quit") ;
#endif

	button = (pan_threshold != 0.0 ? -1 : 0);
	R_get_location_with_pointer (&screen_x, &screen_y, &button);
	flush_keyboard (); /*ADDED*/
	Clear_info ();

	switch (button)
	{
	    case -1:
		if(pan_threshold == 0.0)
			break;

		screen_to_utm ( screen_x, screen_y, &ux1, &uy1) ;

		tmp1 = pan_threshold * (U_east  - U_west);
		tmp2 = pan_threshold * (U_north - U_south);

		if((ux1 > U_west  + tmp1 && ux1 < U_east  - tmp1 &&
		    uy1 > U_south + tmp2 && uy1 < U_north - tmp2) ||
		   (ux1 < U_west  || ux1 > U_east ||
		    uy1 < U_south || uy1 > U_north))
			break;

		tmp3 = (U_east  + U_west)  / 2;
		tmp4 = (U_north + U_south) / 2;

		tmp1 = tmp1 * (ux1 - tmp3) / (U_east  - tmp1 - tmp3);
		tmp2 = tmp2 * (uy1 - tmp4) / (U_north - tmp2 - tmp4);

		W = U_west  + tmp1;
		E = U_east  + tmp1;
		S = U_south + tmp2;
		N = U_north + tmp2;

                clear_window ();
	        window_rout (N, S, E, W);
		Clear_base ();
	        replot(CMap);
		if(Xpoints)
			highlight_line (type, Xpoints, 0, NULL);
		Clear_info();

		break;

	    case LEFTB:
	    case MIDDLEB:
		screen_to_utm ( screen_x, screen_y, &ux1, &uy1) ;
		tmp1 =  (ux1 - ((U_east  + U_west)  / 2));
		tmp2 =  (uy1 - ((U_north + U_south) / 2));
		W = U_west  + tmp1;
		E = U_east  + tmp1;
		S = U_south + tmp2;
		N = U_north + tmp2;

                clear_window ();
	        window_rout (N, S, E, W);
		Clear_base ();
	        replot(CMap);
		if(Xpoints)
			highlight_line (type, Xpoints, 0, NULL);
		Clear_info();

		break;

	    case RIGHTB:
		return (0);
		break;


	default:
	        return(1) ;
	        break ;
	} /* end of switch */
    } /* end of while */
}
