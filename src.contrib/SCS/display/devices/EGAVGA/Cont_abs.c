/* Function: Cont_abs		P.W. Carlson		April 1990	*/

#include "driver.h"

Cont_abs(x, y)
int x, y;
{
    cur_x = x;
    cur_y = y;

    put_chr('d');
    put_int(x);
    put_int(y);
}
