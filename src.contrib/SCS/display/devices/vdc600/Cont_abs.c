/* Function: Cont_abs		P.W. Carlson		1/90	*/

#include <sys/types.h>
#include <sys/at_ansi.h>
#include <sys/kd.h>
#include "vdc600.h"

Cont_abs(end_x, end_y)
int end_x, end_y;
{
    register int dx, dy;
    int xinc, yinc, err, errinc, errdec;
    
    /* compute delta x, delta y, and x and y increments */
    dx = end_x - current.x;
    if (dx > 0) xinc = 1;
    else
    {	xinc = -1;
    	dx = -dx;
    }
    dy = end_y - current.y;
    if (dy > 0) yinc = H_RES;
    else
    {	yinc = -H_RES;
    	dy = -dy;
    }
    
    /* if delta x is greater than delta y */
    if (dx > dy)
    {	errinc = dy + dy;
    	errdec = errinc - dx - dx;
	err = errinc - dx;
	for (dx++; ; dx--)
	{   CHECK_SEG();
	    *(graphics_base + video.segment.offset) = current.color;
	    if (dx == 1) break;
	    video.total_offset += xinc;
	    if (err < 0) err += errinc;
	    else
	    {	video.total_offset += yinc;
	    	err += errdec;
	    }
	}
    }
    
    /* if delta x is less or equal to delta y */
    else
    {	errinc = dx + dx;
    	errdec = errinc - dy - dy;
	err = errinc - dy;
	for (dy++; ; dy--)
	{   CHECK_SEG();
	    *(graphics_base + video.segment.offset) = current.color;
	    if (dy == 1) break;
	    video.total_offset += yinc;
	    if (err < 0) err += errinc;
	    else
	    {	video.total_offset += xinc;
	    	err += errdec;
	    }
	}
    }
    
    /* update current coordinates */
    current.x = end_x;
    current.y = end_y;
}
