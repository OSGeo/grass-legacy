/* Function: Polygon_rel    P.W. Carlson		April 1990	*/

#include <stdio.h>
#include "driver.h"

Polygon_rel(xarray, yarray, number)
int *xarray, *yarray, number;
{
    int first_x, first_y, num, *xptr, *yptr;

    num = number;
    xptr = xarray;
    yptr = yarray;

    put_chr('f');
    put_int(num);
    first_x = cur_x + *xptr++;
    first_y = cur_y + *yptr++;
    put_int(first_x);
    put_int(first_y);

    while(--num)
    {   cur_x += *xptr++;
        cur_y += *yptr++;
        put_int(cur_x);
        put_int(cur_y);
    }
}
