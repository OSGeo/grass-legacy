/* %W% %G% */
static double t, b, l, r ;
static int line_size ;

Set_window(T, B, L, R)
	int T, B, L, R ;
{
	t = (double)T ;
	b = (double)B ;
	l = (double)L ;
	r = (double)R ;
}

window_clip(new_x, new_y, cur_x, cur_y)
	double *new_x, *new_y, *cur_x, *cur_y ;
{
	return(clip(t, b, l, r, new_x, new_y, cur_x, cur_y)) ;
}
