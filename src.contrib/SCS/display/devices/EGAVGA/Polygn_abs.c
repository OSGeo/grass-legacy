/* Function: Polygon_abs    	P.W. Carlson		April 1990 */

#include <stdio.h>
#include "driver.h"

Polygon_abs(xarray, yarray, number)
int xarray[], yarray[], number;
{
    int x, y, num, *xptr, *yptr;

    put_chr('f');
    put_int(number);
    for (num = 0; num < number; num++)
    {	put_int(xarray[num]);
    	put_int(yarray[num]);
    }
    cur_x = xarray[number - 1];
    cur_y = yarray[number - 1];
}
