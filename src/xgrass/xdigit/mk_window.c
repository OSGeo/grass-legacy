/*
**  Written by Dave Gerdes  5/1988
**  US Army Construction Engineering Research Lab
*/

#include "digit.h"
#include "dig_head.h"
#include "wind.h"
#include "color.h"

double sN, sS, sE, sW;

set_window_w_mouse ()
{
    char buffer[64] ;
    double x, y ;
    int screen_x, screen_y ;
    double ux1, uy1 ;
    double ux2, uy2 ;
    double sx1, sy1 ;
    double sx2, sy2 ;
    double tmpx, tmpy;
    int button = 0;
    int cur_screen_x, cur_screen_y ;
    double N, S, E, W;
    double delta;
    int yn, box;
    int change = 1;


    cur_screen_x = (int)D_west ;
    cur_screen_y = (int)D_south ;
    screen_x = (int)D_east ;
    screen_y = (int)D_north ;

    sx1 = ux1 = U_west ;
    sy1 = uy1 = U_south ;
    sx2 = ux2 = U_east ;
    sy2 = uy2 = U_north ;
top:
    box = 0;
    show_select_dialog("accept","abort", "Select new window", 1);
    while (1)
    {

	get_location_with_box(&cur_screen_x, &cur_screen_y, &screen_x, &screen_y, &button) ;
	switch (button) {
	    case FIND:
		if ( cur_screen_x == screen_x  ||  cur_screen_y == screen_y)
		{
		    make_monolog(2, "Window is too small to display") ;
		    continue ;
		}
		screen_to_utm ( cur_screen_x, cur_screen_y, &ux1, &uy1) ;
		screen_to_utm ( screen_x, screen_y, &ux2, &uy2) ;
		box = 1;
		break;

	    case ACCEPT:
		if (box)
		{
                    change = 0;
		    goto foo;
		}
		else 
		{
		   copy_pix();
                   show_select_dialog("accept","abort", "Select new window", 1);
		   continue ;
		}

		break;
	    case DONE:
		if (CurrPL)
    		    display_plseg(CurrPL, XD_WHITE);
		return (change);
		break;
	    default:
		goto foo;
		break;
	}

    }

foo:

	    W  =  ux1<ux2?ux1:ux2 ;
	    E  =  ux1>ux2?ux1:ux2 ;
	    S =  uy1<uy2?uy1:uy2 ;
	    N =  uy1>uy2?uy1:uy2 ;
	    if (E < CM->head.W || W > CM->head.E || N < CM->head.S || S > CM->head.N)
	    {
		if (!(yn = mouse_yes_no 
		    ("Window is outside of default. Proceed? ")))
		    goto top;
	    }
	    window_rout (N, S, E, W);
	    redraw();
	    return(0) ;
}

draw_default_window (N, S, E, W)
    double N, S, E, W;
{
    static int dummy;
    static XPoint points[4];
    static XPoint old[5];
    int east, north, south, west;

    if (dummy == 0)
    {
	utm_to_screen ( CM->head.W, CM->head.N, &west, &north);
	utm_to_screen ( CM->head.E, CM->head.S, &east, &south);
	
	points[0].x = (short)west; points[0].y = (short)north;
	points[1].x = (short)east; points[1].y = (short)north;
	points[2].x = (short)east; points[2].y = (short)south;
	points[3].x = (short)west; points[3].y = (short)south;
	dummy = 1;
    }
    standard_color (dcolors[XD_AQUA]);
    polygon_abs (points, 4);

    utm_to_screen ( W, N, &west, &north) ;
    utm_to_screen ( E, S, &east, &south) ;
    
    old[0].x = (short)west; old[0].y = (short)north;
    old[1].x = (short)east; old[1].y = (short)north;
    old[2].x = (short)east; old[2].y = (short)south;
    old[3].x = (short)west; old[3].y = (short)south;
    old[4].x = old[0].x;    old[4].y = old[0].y;

    standard_color (dcolors[XD_RED]);
    polyline_abs (old, 5, 0);


}

set_window_w_box ()
{
   display_plseg(CurrPL, XD_WHITE);
   set_window_w_mouse();
}
Widen ()
{
    double x, y ;
    int screen_x, screen_y ;
    double ux1, uy1 ;
    double ux2, uy2 ;
    double sx1, sy1 ;
    double sx2, sy2 ;
    int cur_screen_x, cur_screen_y ;
    double N, S, E, W;
    double delta;


    sN = U_north; sW = U_west; sE = U_east; sS = U_south;

    Box = 1;
    clear_window ();
    W  =  CM->head.W;
    E  =  CM->head.E;
    S  =  CM->head.S;
    N  =  CM->head.N;
    delta = (CM->head.E - CM->head.W);
    W -= delta;
    E += delta;
    delta = (CM->head.N - CM->head.S);
    N += delta;
    S -= delta;

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
		replot (CM);
		*/
/*DEBUG*/ debugf ("MKWIND default (%lf, %lf, %lf, %lf)\n", sN, sS, sE, sW);
    draw_default_window (sN, sS, sE, sW);
    if(set_window_w_mouse())
    {
        window_rout (sN, sS, sE, sW);
        redraw();
    }
    Box = 0;
}
draw_expose_default()
{
    clear_window();
    draw_default_window (sN, sS, sE, sW);
    fill_pix();
}
