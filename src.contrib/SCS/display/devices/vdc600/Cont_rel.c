/* Function: Cont_rel		P.W. Carlson		1/90	*/

#include "vdc600.h"

Cont_rel(delta_x, delta_y)
int delta_x, delta_y;
{
    Cont_abs(current.x + delta_x, current.y + delta_y);
    current.x += delta_x;
    current.y += delta_y;
}
