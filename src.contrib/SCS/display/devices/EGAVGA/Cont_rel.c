/* Function: Cont_rel		P.W. Carlson		April 1990	*/

#include "driver.h"

Cont_rel(x,y)
int x, y;
{
    Cont_abs(cur_x + x, cur_y + y );
}
