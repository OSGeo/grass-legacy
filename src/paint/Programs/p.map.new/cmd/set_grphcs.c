#include "fullwindow.h"
#include "graphics.h"
#include "gis.h"

int init_graphics (int pcols)
{
    int i;
    unsigned char *rp;

    graphics.raster_size = (pcols) * (RASTER_ROWS);
    rp  = (unsigned char *)G_malloc (graphics.raster_size);
    for (i = 0; i < RASTER_ROWS; i++)
    {
	graphics.raster[i] = rp;
	rp += pcols;
    }

    return 0;
}

int set_graphics (struct Cell_head *window, int row, int col)
{
    graphics.color         = 0;
    graphics.width1        = 1;
    graphics.width2        = 0;
    graphics.drawover      = 1;
    graphics.window.top    = row ;
    graphics.window.bottom = row + RASTER_ROWS - 1  ;
    if(graphics.window.bottom >= fullwindow.rows)
	    graphics.window.bottom = fullwindow.rows - 1;
	
    graphics.window.left   = col;
    graphics.window.right  = col + window->cols - 1;
    graphics.window.north  = window->north ;
    graphics.window.south  = window->south;
    graphics.window.east   = window->east ;
    graphics.window.west   = window->west ;
    graphics.window.ns_res = window->ns_res ;
    graphics.window.ew_res = window->ew_res ;


#ifdef DEBUG
fprintf (stdout,"\nset window: row = %d\n", row);
fprintf (stdout," north %10lf south  %10lf\n", graphics.window.north, graphics.window.south);
fprintf (stdout," east  %10lf west   %10lf\n", graphics.window.east, graphics.window.west);
fprintf (stdout," top   %10d bottom %10d\n", graphics.window.top, graphics.window.bottom);
fprintf (stdout," left  %10d right  %10d\n", graphics.window.left, graphics.window.right);
#endif


    G_zero (*graphics.raster, graphics.raster_size);
    graphics.dirty = 0;



    return row + RASTER_ROWS;
}
