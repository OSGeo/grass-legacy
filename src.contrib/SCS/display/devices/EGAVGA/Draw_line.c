/* Function: draw_line		P.W. Carlson		April 1990	*/

#include "driver.h"

draw_line(x1, y1, x2, y2)
int x1, y1, x2, y2;
{
    Move_abs(x1, y1);
    Cont_abs(x2, y2);
}
