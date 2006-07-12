#include "driver.h"
#include "driverlib.h"

static double t, b, l, r;

void COM_Set_window(int T, int B, int L, int R)
{
	t = (double) T;
	b = (double) B;
	l = (double) L;
	r = (double) R;
}

int window_clip(double *new_x, double *new_y, double *cur_x, double *cur_y)
{
	return clip(t, b, l, r, new_x, new_y, cur_x, cur_y);
}

int window_box_clip(double *x1, double *y1, double *x2, double *y2)
{
	int clipped = 0;

	if(*x1 < l){
		*x1 = l;
		clipped = 1;
	}
	if(*x2 > r){
		*x2 = r;
		clipped = 1;
	}
	if(*y1 < t){
		*y1 = t;
		clipped = 1;
	}
	if(*y2 > b){
		*y2 = b;
		clipped = 1;
	}

	return clipped;
}
