/* Functions: Move_abs, Get_current_xy    	P.W. Carlson	April 1990  */

#include "driver.h"

Move_abs(x,y)
    int x, y;
{
    cur_x = x;
    cur_y = y;

    put_chr('M');
    put_int(x);
    put_int(y);
}


Get_current_xy(x, y)
    int *x, *y;
{
    *x = cur_x;
    *y = cur_y;
}

