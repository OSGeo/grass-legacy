/* Function: Move_rel		P.W. Carlson		1/90		*/

#include <sys/types.h>
#include <sys/at_ansi.h>
#include <sys/kd.h>
#include "vdc600.h"

Move_rel(delta_x, delta_y)
int delta_x, delta_y;
{
    Move_abs(current.x + delta_x, current.y + delta_y);
    current.x += delta_x;
    current.y += delta_y;
}
