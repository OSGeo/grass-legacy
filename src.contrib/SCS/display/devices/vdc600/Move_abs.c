/* Functions: Move_abs, Get_current_xy		P.W. Carlson	1/90	*/

#include <sys/types.h>
#include <sys/at_ansi.h>
#include <sys/kd.h>
#include "vdc600.h"

Move_abs(x, y)
int x, y;
{
    /* set the current coordinates */
    current.x = x;
    current.y = y;
    
    /* compute the video segment number and offset */
    video.total_offset = H_RES * current.y + current.x;
    CHECK_SEG();
}


Get_current_xy(x, y)
int *x, *y;
{
    *x = current.x;
    *y = current.y;
}

