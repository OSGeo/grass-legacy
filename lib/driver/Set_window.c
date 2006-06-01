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

