/*
**  Written by Dave Gerdes  5/1988
**  US Army Construction Engineering Research Lab
*/

#include "digit.h"
#include "debug.h"
#include "raster.h"
#include "dig_curses.h"
#include "wind.h"
#include "local_proto.h"
#include "glocale.h"

int 
set_window_w_mouse (void)
{
    char buffer[64] ;
    double x, y ;
    int screen_x, screen_y ;
    double ux1, uy1 ;
    double ux2, uy2 ;
    double sx1, sy1 ;
    double sx2, sy2 ;
    int button ;
    int cur_screen_x, cur_screen_y;
    double N, S, E, W, ns, ew, n, s;
    double delta;
    int yn;
    int mode;


    cur_screen_x = (int)D_west ;
    cur_screen_y = (int)D_south ;
    screen_x = (int)D_east ;
    screen_y = (int)D_north ;
/*
    screen_x = cur_screen_x + 10 ;
    screen_y = cur_screen_y + 10 ;
*/
    sx1 = ux1 = U_west ;
    sy1 = uy1 = U_south ;
    sx2 = ux2 = U_east ;
    sy2 = uy2 = U_north ;
top:

    mode = 1;
    while (1)
    {
	
	Clear_info ();
	_Clear_base ();
	_Write_base (12, _("   Buttons:")) ;
	if ( mode == 1 ) {
	    _Write_base (13, _("   Left:   1. corner")) ;
	    _Write_base (14, _("   Middle: Unzoom")) ;
	    Write_base  (15, _("   Right:  Main zoom menu")) ;
	} else {
	    _Write_base (13, _("   Left:   1. corner (reset)")) ;
	    _Write_base (14, _("   Middle: 2. corner")) ;
	    Write_base  (15, _("   Right:  Main zoom menu")) ;
	}

        if ( mode == 1 ) {
            R_get_location_with_pointer(&screen_x, &screen_y, &button);
	}
	else
	    R_get_location_with_box(cur_screen_x, cur_screen_y, &screen_x, &screen_y, &button) ;

	switch (button) {
	    case LEFTB:
		cur_screen_x = screen_x ;
		cur_screen_y = screen_y ;
		screen_to_utm ( cur_screen_x, cur_screen_y, &ux1, &uy1) ;
		mode = 2;
		break;

	    case MIDDLEB:
		screen_to_utm ( screen_x, screen_y, &ux2, &uy2) ;
		clear_window ();
		switch ( mode ) {
		    /* window */
	            case ( 2 ) : 
			if ( cur_screen_x == screen_x  ||  cur_screen_y == screen_y)
			{
			    Write_info(2, _("Window is too small to display")) ;
			    continue ;
			}
			W  =  ux1<ux2?ux1:ux2 ;
			E  =  ux1>ux2?ux1:ux2 ;
			S =  uy1<uy2?uy1:uy2 ;
			N =  uy1>uy2?uy1:uy2 ;
			if (E < CMap->head.W || W > CMap->head.E || N < CMap->head.S || S > CMap->head.N)
			{
			    yn = curses_yes_no_default (2,_("Window is outside of default. Proceed? "), 1);
			    Clear_info ();
			    if (!yn)
				goto top;
			}
			window_rout (N, S, E, W);
			mode = 1;
			break;
		    /* unzoom */
		    case ( 1 ) : 
			ns = U_north - U_south;
			ew = U_east - U_west;
			N = uy2 + ns;
			S = uy2 - ns;
			E = ux2 + ew;
			W = ux2 - ew;
			window_rout (N, S, E, W);
			break;
		}
		replot(CMap);
		break;
	    case RIGHTB:
	        return(0) ;
		break;
	}
    }

	    return(0) ;
}

int 
draw_default_window (double N, double S, double E, double W)
{
    static int dummy;
    static int screenx [4];
    static int screeny [4];
    static int oldx [5];
    static int oldy [5];

    if (dummy == 0)
    {
	utm_to_screen ( CMap->head.W, CMap->head.N, screenx, screeny) ;
	utm_to_screen ( CMap->head.E, CMap->head.N, screenx+1, screeny+1) ;
	utm_to_screen ( CMap->head.E, CMap->head.S, screenx+2, screeny+2) ;
	utm_to_screen ( CMap->head.W, CMap->head.S, screenx+3, screeny+3) ;
	dummy = 1;
    }
    R_standard_color (dcolors[AQUA]);
    R_polygon_abs (screenx, screeny, 4);

    utm_to_screen ( W, N, oldx, oldy) ;
    utm_to_screen ( E, N, oldx+1, oldy+1) ;
    utm_to_screen ( E, S, oldx+2, oldy+2) ;
    utm_to_screen ( W, S, oldx+3, oldy+3) ;
    oldx[4] = oldx[0];
    oldy[4] = oldy[0];

    R_standard_color (dcolors[RED]);
    R_polyline_abs (oldx, oldy, 5);

    V_flush ();

    return 0;
}
