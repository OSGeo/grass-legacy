
/*
 * draw a line between two given points in the current color.
 *
 * Called by:
 *     Cont_abs() in ../lib/Cont_abs.c
 *
 *  Written by the GRASS Team in the Winter of 88.
 *
 */

 extern  int  WNO ;

draw_line(cur_x, cur_y, x, y)
{
	Hide_cursor();
	move(WNO, cur_x, cur_y) ;
	draw(WNO, x, y) ;
	Show_cursor();
}
