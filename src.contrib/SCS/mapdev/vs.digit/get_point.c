/* modified by RL Glenn  12/1991
** USDA, SCS, Tech. Infor. Sys. Division
*/
#include "digit.h"


get_point (x, y, str)
    double *x, *y;
    char *str;
{
	new_point_with_mouse (x, y, str);
	if (0.0 == *x && 0.0 == *y)
	    return -2;
	else
	    return -1;
}

