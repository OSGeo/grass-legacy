/* Function: draw_line		P.W. Carlson		1/90	*/

#include "vdc600.h"

draw_line(x1, y1, x2, y2)
int x1, y1, x2, y2;
{
    if (y1 == y2 && x2 > x1) horline(x1, y1, x2, y2);
    else
    {	Move_abs(x1, y1);
	Cont_abs(x2, y2);
    }
}
