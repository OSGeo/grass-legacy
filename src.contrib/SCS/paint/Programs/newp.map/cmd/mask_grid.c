
#include "gis.h"
#include "graphics.h"
#include "parms.h"
#include "grid.h"
mask_grid()
{
    int rasrow;
    int row;
    register unsigned char *ras;
    register CELL *c;
    register CELL *cbuf;
    register int col;
	int masked = 0;

	if (strcmp (grid.gridon, "all") == NULL)
		return;

	if (strcmp (grid.gridon, "data") == NULL)
		masked = 1;

    if (!graphics.dirty)
	return;

printf (" mask grid \n");
	cbuf = NULL;
	cbuf = G_allocate_cell_buf();

    rasrow = 0;
    for (row = graphics.window.top; row <= graphics.window.bottom; row++)
    {
	G_get_map_row (parms.cellfd, c = cbuf, row);

	col = G_window_cols();
	ras = graphics.raster[rasrow++];
	while (col-- > 0)
	{
	if (masked) {
	    if (*c++ == 0)
		*ras = 0;
		}
	else {
	    if (*c++ != 0)
		*ras = 0;
		}
	    ras++;
	}
    }

printf (" end of mask grid \n");
}
