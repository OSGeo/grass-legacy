#include "../XDRIVER.h"
#include "driver.h"

int Move_rel (int x, int y)
{
    Move_abs(cur_x + x, cur_y + y);
    return 0;
}
