/* Functions: Box_abs, Box_rel		P.W. Carlson 	1/90	*/

#include <memory.h>
#include <sys/types.h>
#include <sys/at_ansi.h>
#include <sys/kd.h>
#include "vdc600.h"

Box_abs(x1, y1, x2, y2)
int x1, y1, x2, y2;
{
    register int y; 
    int tmp, npixels, pixtoend;

    if (x1 > x2)
    { 	tmp = x1; 
	x1 = x2; 
	x2 = tmp; 
    }
    if (y1 > y2)
    { 	tmp = y1; 
	y1 = y2; 
	y2 = tmp; 
    }

    npixels = x2 - x1 + 1;
    if (npixels > 0)
    {	for (y = y1; y <= y2; y++)
        {   video.total_offset = H_RES * y + x1;
	    CHECK_SEG();
    	    pixtoend = 0x10000 - video.segment.offset;
            if (npixels <= pixtoend)
                memset(graphics_base + video.segment.offset, current.color,
			npixels);
    	    else 
            {   memset(graphics_base + video.segment.offset, current.color,
			pixtoend);
	        video.segment.number++;
	        CHECK_SEG();
                memset(graphics_base, current.color, npixels - pixtoend);
            }
	}
    }
}


Box_rel(x1, y1, x2, y2)
int x1, y1, x2, y2;
{
    /* convert to absolute coords. and call Box_abs */
    y1 += current.y;
    y2 += current.y;
    x1 += current.x;
    x2 += current.x;
    Box_abs(y1, y2, x1, x2);
}
