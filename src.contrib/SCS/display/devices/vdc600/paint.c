/* Function: paint
**
** This function paints an area with the current color.
** Area boundaries are any pixels with non-zero colors.
**
** Author: Paul W. Carlson	Jan. 1990
*/

#include <memory.h>
#include <sys/types.h>
#include <sys/at_ansi.h>
#include <sys/kd.h>
#include "vdc600.h"

#define TESTPIX(X, Y) (X >= 0 && X < H_RES && Y >= 0 && Y < V_RES && !raster[X])

static unsigned char *ptr;
 
paint(x, y)
int x, y;
{
    register int start_leftx, start_rightx;
    unsigned char raster[H_RES];

    getrast(y, raster);
    if (!(TESTPIX(x, y))) return(0);
    start_leftx = x;
    start_rightx = x;
    ptr = raster + start_leftx - 1;
    while (start_leftx - 1 >= 0 && *ptr == 0) 
    {   ptr--;
    	start_leftx--;
    }
    ptr = raster + start_rightx + 1;
    while (start_rightx + 1 <= H_RES && *ptr == 0) 
    {   ptr++;
    	start_rightx++;
    }
    horline(start_leftx, start_rightx, y);
    recurfill(start_leftx, start_rightx, y - 1, 
              start_leftx, start_rightx, 1);
    recurfill(start_leftx, start_rightx, y + 1, 
    	      start_leftx, start_rightx, -1);
}


recurfill(leftx, rightx, y, parent_leftx, parent_rightx, direction)
int leftx, rightx, y, parent_leftx, parent_rightx, direction;
{
    register int next_leftx, next_rightx;
    unsigned char raster[H_RES];

    getrast(y, raster);
    next_leftx = leftx;
    do
    {   if (TESTPIX(next_leftx, y))
        {   next_rightx = next_leftx;
    	    ptr = raster + next_leftx - 1;
    	    while (next_leftx - 1 >= 0 && *ptr == 0) 
    	    {   ptr--;
    		next_leftx--;
    	    }
	}
        else
        {   next_leftx++;
            while (!(TESTPIX(next_leftx, y)) && 
		(next_leftx <= rightx)) next_leftx++;
	    next_rightx = next_leftx;
        }
        if (next_leftx <= rightx)
    	{   ptr = raster + next_rightx + 1;
    	    while (next_rightx + 1 <= H_RES && *ptr == 0) 
    	    {   ptr++;
    		next_rightx++;
      	    }
	    horline(next_leftx, next_rightx, y);
	    recurfill(next_leftx, next_rightx, y - direction, 
	    	      next_leftx, next_rightx, direction);
	    if (next_leftx <= parent_leftx - 2) 
	        recurfill(next_leftx, parent_leftx - 2, y + direction, 
			  next_leftx, next_rightx, -direction);
	    if (parent_rightx + 2 <= next_rightx) 
	        recurfill(parent_rightx + 2, next_rightx, y + direction, 
			  next_leftx, next_rightx, -direction);
	    next_leftx = next_rightx + 2;
        }
    } while (next_leftx <= rightx);
}


static getrast(y, raster)
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
