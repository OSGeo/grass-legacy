#include "sun.h"

/*
 * draw a line between two given points in the current color.
 *
 * Called by:
 *     Cont_abs() in ../lib/Cont_abs.c
 */

static int draw_count = 0 ;
extern int SCREEN_BOTTOM ;

draw_line(cur_x, cur_y, x, y)
{
	/* Draw so many vectors before closing temporary segment */

	if(draw_count > 512)
	{
		close_temporary_segment() ;
		create_temporary_segment() ;
		draw_count = 0 ;
	}

	if (sun_x != cur_x || sun_y != cur_y)
		move_abs_2((float)cur_x, (float)(SCREEN_BOTTOM - cur_y)) ;

	line_abs_2((float)x, (float)(SCREEN_BOTTOM - y)) ;
	sun_x = x ;
	sun_y = y ;
	draw_count++ ;
}
