#include "../XDRIVER.h"
#include "driver.h"

int Move_abs (int x, int y)
{
    cur_x = x;
    cur_y = y;

    return 0;
}

int Get_current_xy (int *x, int *y)
{
    *x = cur_x;
    *y = cur_y;

    return 0;
}
