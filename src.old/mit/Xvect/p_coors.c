/* @(#)p_coors.c	2.2   7/24/87 */

#include "digit.h"
#include "dig_conv.h"
#include <X11/Xlib.h>
#include <X11/Xutil.h>


#define XADJ(x)    (int)((x - dig_U_west ) * dig_U_to_D_xconv + xl)
#define YADJ(y)		(int)((dig_U_north - y) * dig_U_to_D_yconv  + yt)


dig_plot_all_coors(line_p)
	struct  line_pnts  *line_p ;
{
	int i ;
	int n_coors ;
	double *xptr, *yptr ;
	double utm_x, utm_y;
	XPoint *pts_ptr, *a_pt;
	unsigned int incr;
	extern Display *the_display;
	extern Window the_window;
	extern GC the_gc;

	/* printf("\n coors"); */
	n_coors = line_p->n_points ;
	xptr = line_p->x ;
	yptr = line_p->y ;

	/*
	dig_First(xptr++, yptr++) ;

	for(i=1; i<n_coors; i++)
		dig_Next(xptr++, yptr++) ;
		*/
	
	incr = sizeof(XPoint);

	pts_ptr = (XPoint *) 
		G_malloc(n_coors * incr);

	for( i = 0; i < n_coors; i++)
	{
	a_pt = (pts_ptr + i);

	utm_x = *(xptr + i);
	utm_y = *(yptr + i);

	a_pt->x = (short) XADJ(utm_x);
	a_pt->y = (short) YADJ(utm_y);

	/* printf("\n x = %d, y = %d", a_pt->x, a_pt->y); */
	}

	XDrawLines(the_display, the_window, the_gc,
		pts_ptr, n_coors, CoordModeOrigin);


}


static double current_x_utm, current_y_utm ;
static on_a_roll ;

dig_First(x, y)
	double *x, *y ;
{
	current_x_utm = *x ;
	current_y_utm = *y ;
	on_a_roll = 0 ;
}

dig_Next(x, y)
	double *x, *y ;
{
	int off_a_roll ;
	int xpos, ypos ;
	double new_x, new_y ;

/* check to see if entire line segment is outside window */

	if ((*x < dig_U_west) && (current_x_utm < dig_U_west))
		off_a_roll = 1 ;
	else if ((*x > dig_U_east) && (current_x_utm > dig_U_east))
		off_a_roll = 1 ;
	else if ((*y < dig_U_south) && (current_y_utm < dig_U_south))
		off_a_roll = 1 ;
	else if ((*y > dig_U_north) && (current_y_utm > dig_U_north))
		off_a_roll = 1 ;
	else
		off_a_roll = 0 ;


	if (off_a_roll)
	{
		on_a_roll = 0 ;
	}
	else
	{
		new_x = *x ;
		new_y = *y ;
		D_clip( dig_U_south, dig_U_north, dig_U_west, dig_U_east, 
			&new_x, &new_y, &current_x_utm, &current_y_utm) ;
			
		if (on_a_roll)
		{
			xpos = XADJ(new_x) ;
			ypos = YADJ(new_y) ;
			R_cont_abs(xpos, ypos) ;
		}
		else
		{
			xpos = XADJ(current_x_utm) ;
			ypos = YADJ(current_y_utm) ;
			R_move_abs(xpos, ypos) ;
			xpos = XADJ(new_x) ;
			ypos = YADJ(new_y) ;
			R_cont_abs(xpos, ypos) ;
			on_a_roll = 1 ;
		}
	}
	current_x_utm = *x ;
	current_y_utm = *y ;

	return(0) ;
}

