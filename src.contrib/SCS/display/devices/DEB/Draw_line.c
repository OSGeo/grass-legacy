draw_line(x1, y1, x2, y2)
	int x1, y1, x2, y2;
{
	extern int cur_color, cur_x, cur_y;

	line(x1, y1, x2, y2, cur_color);
	cur_x = x2;
	cur_y = y2;
}
