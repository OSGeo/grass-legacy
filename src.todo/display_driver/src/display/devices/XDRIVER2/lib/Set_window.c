#include "../XDRIVER.h"

static double t, b, l, r;

int Set_window (int T, int B, int L, int R)
{
    t = (double) T;
    b = (double) B;
    l = (double) L;
    r = (double) R;
	/*
	fprintf (stdout,"Set Window - t is %d \n", t);
	fprintf (stdout,"Set Window - b is %d \n", b);
	fprintf (stdout,"Set Window - l is %d \n", l);
	fprintf (stdout,"Set Window - r is %d \n", r);
	*/
    return 0;
}

int window_clip (double *new_x, double *new_y, double *cur_x, double *cur_y)
{
/*
fprintf (stdout," window clip - new_x is %d \n", new_x);
fprintf (stdout," window clip - new_y is %d \n", new_y);
fprintf (stdout," window clip - cur_x is %d \n", cur_x);
fprintf (stdout," window clip - cur_y is %d \n", cur_y);
*/
    return (clip(t, b, l, r, new_x, new_y, cur_x, cur_y));
}
