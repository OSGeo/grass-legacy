#include "graphics.h"
#include "misc.h"
#include "sgrid.h"

int dot (int x, int y)
{
    if (x < graphics.window.left || x > graphics.window.right)  return 0;
    if (y < graphics.window.top || y > graphics.window.bottom) return 0;


    if (m_set_grid )
    {
        x -= graphics.window.left;
        y -= graphics.window.top ;
        if (x < dcols || y < drows)
        { 
            graphics.raster[y][x] = 0;
            graphics.dirty = 1;
            return 0;
	}
	else 
	{
            if(graphics.drawover || graphics.raster[y][x] == 0)
            {
 	       graphics.raster[y][x] = graphics.color;
	       graphics.dirty = 1;
            }
	    return 0;
	}

    } /* m_set_grid */


    if (!set_num && set_grid_num) 
    {
        x -= graphics.window.left;
	x +=pcols_sp;
        y -= graphics.window.top ;
	y += prows_sp;
    }
    else 
    {
        x -= graphics.window.left;
        y -= graphics.window.top ;
	if (set_vert) 
	     x +=pcols_sp;
	if (set_hort) 
	     y += prows_sp;
     }

     if(graphics.drawover || graphics.raster[y][x] == 0)
     {
 	  graphics.raster[y][x] = graphics.color;
          graphics.dirty = 1;
     } 

     return 0;
}
