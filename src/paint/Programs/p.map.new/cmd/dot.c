#include "graphics.h"
#include "misc.h"
#include "sgrid.h"
dot (x,y)
{
    if (x < graphics.window.left || x > graphics.window.right)  return;
    if (y < graphics.window.top || y > graphics.window.bottom) return;


    if (m_set_grid )
    {
        x -= graphics.window.left;
        y -= graphics.window.top ;
        if (x < dcols || y < drows)
        { 
            graphics.raster[y][x] = 0;
            graphics.dirty = 1;
            return;
	}
	else 
	{
            if(graphics.drawover || graphics.raster[y][x] == 0)
            {
 	       graphics.raster[y][x] = graphics.color;
	       graphics.dirty = 1;
            }
	    return;
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
}
