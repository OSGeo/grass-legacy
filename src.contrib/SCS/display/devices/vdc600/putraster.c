/* Function: putraster
**
** This function writes a block of pixels from the array pointed to
** by the argument "raster."
**
** Author: Paul W. Carlson	Jan. 1990
*/

#include <memory.h>
#include <sys/types.h>
#include <sys/at_ansi.h>
#include <sys/kd.h>
#include "vdc600.h"

putraster(x1, y1, x2, y2, raster)
int x1, y1, x2, y2;
unsigned char *raster;
{
    register int y; 
    int npixels, pixtoend;

    npixels = x2 - x1 + 1;
    if (npixels <= 0) return;
    for (y = y1; y <= y2; y++)
    {	video.total_offset = H_RES * y + x1;
    	CHECK_SEG();
    	pixtoend = 0x10000 - video.segment.offset;
    	if (npixels <= pixtoend)
            memcpy(graphics_base + video.segment.offset, raster, npixels);
    	else 
    	{   memcpy(graphics_base + video.segment.offset, raster, pixtoend);
	    video.segment.number++;
	    CHECK_SEG();
            memcpy(graphics_base, raster + pixtoend, npixels - pixtoend);
        }
    }
}
