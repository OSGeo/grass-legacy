
#include "driver.h"

Move_abs(x, y)
int x, y;
{
    cur_x = x;
    cur_y = y;
}

Get_current_xy(x, y)
int *x, *y;
{
    *x = cur_x;
    *y = cur_y;
}
