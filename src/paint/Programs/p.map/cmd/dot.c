#include "graphics.h"

int dot (int x, int y)
{
    if (x < graphics.window.left || x > graphics.window.right)  return 1;
    if (y < graphics.window.top  || y > graphics.window.bottom) return 1;

    x -= graphics.window.left;
    y -= graphics.window.top;

    if(graphics.drawover || graphics.raster[y][x] == 0)
    {
	graphics.raster[y][x] = graphics.color;
	graphics.dirty = 1;
    }

    return 0;
}
