#define ESC 033

Move_abs(x,y)
	int x, y ;
{
	extern int current_x_pos ;
	extern int current_y_pos ;

	current_x_pos = x ;
	current_y_pos = y ;
	move_abs() ;
}

move_abs_no_update(x,y)
	int x, y ;
{
	extern int current_x_pos ;
	extern int current_y_pos ;

	current_x_pos = x ;
	current_y_pos = y ;
}

Get_current_xy(x, y)
	int *x, *y ;
{
	*x = current_x_pos ;
	*y = current_y_pos ;
}

move_abs()
{
	extern int current_x_pos ;
	extern int current_y_pos ;
	char *encode_xy() ;
	char *res ;

	res = encode_xy(current_x_pos,current_y_pos) ;
	printf("%cLF%c%c%c%c%c", ESC, res[0],res[1],res[2],res[3],res[4]) ;
		return(0) ;
}

is_inside(x, y)
	int x, y ;
{
	extern int SCREEN_LEFT	  ;
	extern int SCREEN_RIGHT  ;
	extern int SCREEN_BOTTOM ;
	extern int SCREEN_TOP    ;
	if (x < SCREEN_LEFT || x > SCREEN_RIGHT ||
	    y > SCREEN_BOTTOM || y < SCREEN_TOP)
	{
		return(0) ;
	}
	
	return(1) ;
}
