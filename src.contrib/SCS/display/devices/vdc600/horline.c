/* Function: horline
**
** Author: Paul W. Carlson		Jan. 1990
*/
#include <memory.h>
#include <sys/types.h>
#include <sys/at_ansi.h>
#include <sys/kd.h>
#include "vdc600.h"

horline(x1, x2, y)
int x1, x2, y;
{
    register int npixels, pixtoend;

    video.total_offset = H_RES * y + x1;
    CHECK_SEG();
    npixels = x2 - x1 + 1;
    if (npixels > 0)
    {	pixtoend = 0x10000 - video.segment.offset;
        if (npixels <= pixtoend)
        {   memset(graphics_base + video.segment.offset, current.color,
		npixels);
	   video.segment.offset = npixels - 1;
	}
    	else 
        {  memset(graphics_base + video.segment.offset, current.color,
		pixtoend);
	   video.segment.number++;
	   CHECK_SEG();
           memset(graphics_base, current.color, npixels - pixtoend);
	   video.segment.offset = npixels - pixtoend - 1;
        }
    }
    current.y = y;
    current.x = x2;
}
