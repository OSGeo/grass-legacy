#define ESC 033
Cont_abs(x,y)
	int x, y ;
{
	extern int current_x_pos ;
	extern int current_y_pos ;
	int cx, cy ;
	int X[4], Y[4];
	int count;
	char *encode_xy() ;
	char *s ;

	if (! is_inside(current_x_pos, current_y_pos))
	{
		move_abs_no_update(x,y) ;
		return ;
	}

	cx = current_x_pos ;
	cy = current_y_pos ;
	if (cy != y)
	{
		X[0] = cx;
		X[1] = x;
		Y[0] = cy;
		Y[1] = y;
		count = 2;
	}
	else
	{
		X[0] = cx;
		X[1] = cx;
		X[2] = x;
		X[3] = x;
		Y[0] = cy;
		Y[1] = cy+1;
		Y[2] = cy+1;
		Y[3] = cy;

		count = 4;
	}

	move_abs_no_update(x,y) ;

	if (! is_inside(current_x_pos, current_y_pos))
		return ;
	Polygon_abs (X, Y, count);
}
