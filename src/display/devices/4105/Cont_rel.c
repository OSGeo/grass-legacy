Cont_rel(x,y)
	int x, y ;
{
	extern int current_x_pos ;
	extern int current_y_pos ;

	Cont_abs(current_x_pos + x, current_y_pos + y ) ;
}
