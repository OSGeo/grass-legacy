#include "graphics.h"
dot (x,y)
{
    if (x < graphics.window.left || x > graphics.window.right)  return;
    if (y < graphics.window.top  || y > graphics.window.bottom) return;

    x -= graphics.window.left;
    y -= graphics.window.top;

    if(graphics.drawover || graphics.raster[y][x] == 0)
    {
	graphics.raster[y][x] = graphics.color;
	graphics.dirty = 1;
    }
}
