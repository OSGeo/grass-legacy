static double t, b, l, r;

Set_window(T, B, L, R)
int T, B, L, R;
{
    t = (double) T;
    b = (double) B;
    l = (double) L;
    r = (double) R;
	/*
	printf ("Set Window - t is %d \n", t);
	printf ("Set Window - b is %d \n", b);
	printf ("Set Window - l is %d \n", l);
	printf ("Set Window - r is %d \n", r);
	*/
}

window_clip(new_x, new_y, cur_x, cur_y)
double *new_x, *new_y, *cur_x, *cur_y;
{
/*
printf (" window clip - new_x is %d \n", new_x);
printf (" window clip - new_y is %d \n", new_y);
printf (" window clip - cur_x is %d \n", cur_x);
printf (" window clip - cur_y is %d \n", cur_y);
*/
    return (clip(t, b, l, r, new_x, new_y, cur_x, cur_y));
}
