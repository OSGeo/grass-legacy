
/*
 * draw a line between two given points in the current color.
 *
 * Called by:
 *     Cont_abs() in ../lib/Cont_abs.c
 */

draw_line(cur_x, cur_y, x, y)
{
	extern int SCREEN_BOTTOM ;
	static int cx = 0 ;
	static int cy = 0 ;

	if (cur_x != cx || cur_y != cy)
	{
		tekmove(cur_x, SCREEN_BOTTOM - cur_y) ;
	}
	tekdraw(x, SCREEN_BOTTOM - y) ;
	cx = cur_x ;
	cy = cur_y ;
}
