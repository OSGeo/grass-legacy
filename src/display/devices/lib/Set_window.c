#include "driverlib.h"
static double t, b, l, r ;
static int line_size ;

int Set_window(int T, int B, int L, int R)
{
	t = (double)T ;
	b = (double)B ;
	l = (double)L ;
	r = (double)R ;

	return 0;
}

int window_clip(double *new_x,double *new_y,double *cur_x,double *cur_y)
{
	return(clip(t, b, l, r, new_x, new_y, cur_x, cur_y)) ;
}
