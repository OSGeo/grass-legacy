
/* load_colors:
** loads our colors into the X colormap
*/

#include "interface.h"

void 
load_colors (data_cell *dc)
{
XColor *c;

    c = &(dc->colors);

/*
    if(XAllocColorCells(dc->dpy, dc->cmap, TRUE, NULL, 0, 
					    dc->cells, MAX_DYN_COLORS)){
*/
	    c->red = 65535;
	    c->green = c->blue = 0;
	    c->pixel = dc->cells[RED_CELL];
	    c->flags = DoRed | DoGreen | DoBlue;
	    XAllocColor(dc->dpy, dc->cmap, c);


	    c->green = 65535;
            c->red = c->blue = 0;
	    c->pixel = dc->cells[GRN_CELL];
	    XAllocColor(dc->dpy, dc->cmap, c);

	    c->blue = 65535;
            c->red = c->green = 0;
	    c->pixel = dc->cells[BLU_CELL];
	    XAllocColor(dc->dpy, dc->cmap, c);
	

	    c->blue = c->red = c->green = 65535;
	    c->pixel = dc->cells[VECT_CELL];	    
	    c->blue = c->red = c->green = 65535;
	    c->pixel = dc->cells[SITES_CELL];
	    XAllocColor(dc->dpy, dc->cmap, c);
	  

	    c->blue = c->red = c->green = 0;
	    c->pixel = dc->cells[BG_CELL];
	    c->red = c->blue = c->green = 4 * 65535 / 5;
	    c->pixel = dc->cells[SURF_CELL];
	    XAllocColor(dc->dpy, dc->cmap, c);
	 

	    c->red = c->blue = c->green = 65535;
	    c->pixel = dc->cells[GRID_CELL];
	    c->green = 0;
            c->blue = c->red = 65535;
	    c->pixel = dc->cells[TMP_CELL];
	    XAllocColor(dc->dpy, dc->cmap, c);

/*
    }

    else 
	fprintf (stdout,"I couldn't allocate the color cells\n"); 

*/

}

