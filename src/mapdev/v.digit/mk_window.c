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
    int cur_screen_x, cur_screen_y ;
    double N, S, E, W;
    double delta;
    int yn;

    Clear_info ();
    _Clear_base ();
    _Write_base (12, "   Buttons:") ;
    _Write_base (13, "      Left:   Establish a corner") ;
#ifdef ANOTHER_BUTTON
    _Write_base (14, "      Middle: Widen view") ;
    Write_base  (15, "      Right:  Accept window") ;
#else
    _Write_base (14, "      Middle: Accept window") ;
    Write_base  (15, "      Right:  Widen view") ;
#endif

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

    while (1)
    {
	R_get_location_with_box(cur_screen_x, cur_screen_y, &screen_x, &screen_y, &button) ;
	Clear_info ();

	switch (button) {
	    case LEFTB:
		cur_screen_x = screen_x ;
		cur_screen_y = screen_y ;
		screen_to_utm ( cur_screen_x, cur_screen_y, &ux1, &uy1) ;
		break;

	    case MIDDLEB:
		if ( cur_screen_x == screen_x  ||  cur_screen_y == screen_y)
		{
		    Write_info(2, "Window is too small to display") ;
		    continue ;
		}
		screen_to_utm ( screen_x, screen_y, &ux2, &uy2) ;
		    goto foo;
		break;

	    case RIGHTB:
	    {
		double sN, sS, sE, sW;

		sN = U_north; sW = U_west; sE = U_east; sS = U_south;

		clear_window ();
		W  =  CMap->head.W;
		E  =  CMap->head.E;
		S  =  CMap->head.S;
		N  =  CMap->head.N;
		delta = (CMap->head.E - CMap->head.W);
		W -= delta;
		E += delta;
		delta = (CMap->head.N - CMap->head.S);
		N += delta;
		S -= delta;
		/*
		window_conversions (N, S, E, W);
		*/
/*DEBUG*/ debugf ("MKWIND (%lf, %lf, %lf, %lf)\n", N, S, E, W);
		window_rout (N, S, E, W);
/*DEBUG*/ debugf ("MKWIND Us after (%lf, %lf, %lf, %lf)\n", U_north, U_south, U_east, U_west);

		cur_screen_x = (int)D_west ;
		cur_screen_y = (int)D_south ;
		screen_x     = (int)D_east ;
		screen_y     = (int)D_north ;
/*DEBUG*/ debugf ("MKWIND screen = (%d, %d, %d, %d)\n", D_north, D_south, D_west, D_east);
/*
		screen_x = cur_screen_x + 10 ;
		screen_y = cur_screen_y + 10 ;
*/
		sx1 = ux1 = U_west ;
		sy1 = uy1 = U_south ;
		sx2 = ux2 = U_east ;
		sy2 = uy2 = U_north ;

		/*
		replot (CMap);
		*/
/*DEBUG*/ debugf ("MKWIND default (%lf, %lf, %lf, %lf)\n", sN, sS, sE, sW);
		draw_default_window (sN, sS, sE, sW);
	    }
		break;
	}

/*
	sprintf(buffer,"NORTH: %10.2f", uy1>uy2?uy1:uy2) ;
	    Write_menu_line(10, buffer) ;
	sprintf(buffer,"SOUTH: %10.2f", uy1<uy2?uy1:uy2) ;
	    Write_menu_line(11, buffer) ;
	sprintf(buffer,"WEST:  %10.2f", ux1<ux2?ux1:ux2) ;
	    Write_menu_line(12, buffer) ;
	sprintf(buffer,"EAST:  %10.2f", ux1>ux2?ux1:ux2) ;
	    Write_menu_line(13, buffer) ;
*/

    }

foo:

	    W  =  ux1<ux2?ux1:ux2 ;
	    E  =  ux1>ux2?ux1:ux2 ;
	    S =  uy1<uy2?uy1:uy2 ;
	    N =  uy1>uy2?uy1:uy2 ;
	    if (E < CMap->head.W || W > CMap->head.E || N < CMap->head.S || S > CMap->head.N)
	    {
		yn = curses_yes_no_default (2,"Window is outside of default. Proceed? ", 1);
		Clear_info ();
		if (!yn)
		    goto top;
	    }
	    clear_window ();
	    /*
	    window_conversions (N, S, E, W);
	    */
	    window_rout (N, S, E, W);
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
