
draw_line(cur_x, cur_y, x, y)
{
	extern int SCREEN_BOTTOM ;
	mgil(cur_x, SCREEN_BOTTOM - cur_y, x, SCREEN_BOTTOM - y) ;
}
