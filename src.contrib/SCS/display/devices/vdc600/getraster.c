/* Function: getraster
**
** This function reads a line of 640 pixels into the array pointed
** to by the argument "raster."
**
** Author: Paul W. Carlson	Jan. 1990
*/

#include <memory.h>
#include <sys/types.h>
#include <sys/at_ansi.h>
#include <sys/kd.h>
#include "vdc600.h"

getraster(y, raster)
int y;
unsigned char *raster;
{
    register int pixtoend;

    video.total_offset = H_RES * y;
    CHECK_SEG();
    pixtoend = 0x10000 - video.segment.offset;
    if (H_RES <= pixtoend)
        memcpy(raster, graphics_base + video.segment.offset, H_RES);
    else 
    {   memcpy(raster, graphics_base + video.segment.offset, pixtoend);
	video.segment.number++;
	CHECK_SEG();
        memcpy(raster + pixtoend, graphics_base, H_RES - pixtoend);
    }
}
