/* Function: Move_rel		P. W. Carlson		April 1990   */

#include "driver.h"

Move_rel(x, y)
int x, y;
{
    Move_abs(cur_x + x, cur_y + y);
}
