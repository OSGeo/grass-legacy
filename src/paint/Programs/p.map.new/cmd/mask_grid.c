#include "gis.h"
#include "graphics.h"
#include "parms.h"
#include "grid.h"
#include "fullwindow.h"
#include "sgrid.h"

int pre_mask_grid (void)
/* added by Olga, apr, 94. This function puts MAXCOLOR+1 on all the empty cells
in masked out area, so that the grid is not drawn there (dots are only drawn
on empty (0) cells). Later on mask_grid() will erase not the whole
masked area with 0 (as it did before) but only the cells = MAXCOLOR + 1,
i.e the cells which were not a part of some drawing other than grid. 
this way all the previous images stay and only the grid is masked out */

{
    int rasrow;
    int row;
    register unsigned char *ras;
    register CELL *c;
    register CELL *cbuf;
    register int col;
    int masked = 0;

    if (strcmp (grid.gridon, "all") == 0)
       	return 0;

    if (strcmp (grid.gridon, "data") == 0)
	masked = 1;

    if (!graphics.dirty)
	return 1;

    cbuf = NULL;
    cbuf = G_allocate_cell_buf();
    rasrow = prows_sp;

    for (row = graphics.window.top; row <= graphics.window.bottom-grows-1; row++)
    {
	int i;
	G_get_map_row (parms.cellfd, c = cbuf, row);

	col = G_window_cols() ;

	ras = graphics.raster[rasrow++];
	for (i=0; i<pcols_sp; i++)
			ras++;

	while (col-- > 0)
	{
	     if (masked) {
	        if (*c++==0 )
		   if(!*ras) *ras = MAXCOLOR + 1;
	     }
	     else {
	        if (*c++ != 0)
		   if(!*ras) *ras = MAXCOLOR + 1;
	     }
	     ras++;
	}
    }
    graphics.drawover = 0;

    return 0;
}

int mask_grid (void)
{
    int rasrow;
    int row;
    register unsigned char *ras;
    register int col;
    int masked = 0;

    if (strcmp (grid.gridon, "all") == 0)
       	return 0;

    if (strcmp (grid.gridon, "data") == 0)
	masked = 1;

    if (!graphics.dirty)
	return 1;

    rasrow = prows_sp;

    for (row = graphics.window.top; row <= graphics.window.bottom-grows-1; row++)
    {
	int i;

	col = G_window_cols() ;

	ras = graphics.raster[rasrow++];
	for (i=0; i<pcols_sp; i++)
			ras++;

	while (col-- > 0)
	{
	     if(*ras > MAXCOLOR) *ras = 0;
	     ras++;
	}
    }
    graphics.drawover = 1;

    return 0;
}
