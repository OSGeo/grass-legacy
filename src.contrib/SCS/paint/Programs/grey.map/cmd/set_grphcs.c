#include "fullwindow.h"
#include "graphics.h"
#include "gis.h"
init_graphics (pcols)
{
    int i;
    unsigned char *rp;

    graphics.raster_size = pcols * RASTER_ROWS;
    rp  = (unsigned char *)G_malloc (graphics.raster_size);
    for (i = 0; i < RASTER_ROWS; i++)
    {
	graphics.raster[i] = rp;
	rp += pcols;
    }
}

set_graphics (window, row, col)
    struct Cell_head *window;
{
    graphics.color         = 0;
    graphics.width1        = 1;
    graphics.width2        = 0;
    graphics.drawover      = 1;
    graphics.window.top    = row;
    graphics.window.bottom = row + RASTER_ROWS - 1;
    if(graphics.window.bottom >= fullwindow.rows)
	    graphics.window.bottom = fullwindow.rows - 1;
    graphics.window.left   = col;
    graphics.window.right  = col + window->cols - 1;
    graphics.window.north  = window->north;
    graphics.window.south  = window->south;
    graphics.window.east   = window->east ;
    graphics.window.west   = window->west ;
    graphics.window.ns_res = window->ns_res ;
    graphics.window.ew_res = window->ew_res ;


#ifdef DEBUG
printf("\nset window: row = %d\n", row);
printf (" north %10lf south  %10lf\n", graphics.window.north, graphics.window.south);
printf (" east  %10lf west   %10lf\n", graphics.window.east, graphics.window.west);
printf (" top   %10d bottom %10d\n", graphics.window.top, graphics.window.bottom);
printf (" left  %10d right  %10d\n", graphics.window.left, graphics.window.right);
#endif

    G_zero (*graphics.raster, graphics.raster_size);
    graphics.dirty = 0;

    return row + RASTER_ROWS;
}
