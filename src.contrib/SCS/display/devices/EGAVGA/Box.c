/* Functions: Box_abs, Box_rel		P.W. Carlson	April 1990 	*/

#include "driver.h"

Box_abs(l, t, r, b)
int l, t, r, b;
{
    int tmp;

    if (l > r)
    {	tmp = l; 
	l = r; 
	r = tmp; 
    }
    if (t > b)
    {	tmp = t; 
	t = b; 
	b = tmp; 
    }

    put_chr('F');
    put_int(l);
    put_int(t);
    put_int(r);
    put_int(b);
}


Box_rel(l, t, r, b)
int l, t, r, b;
{
    t += cur_y;
    b += cur_y;
    l += cur_x;
    r += cur_x;
    Box_abs(t, b, l, r);
}
