#include "gis.h"
#include "graphics.h"
#include "sgrid.h"

int mask_vectors (CELL *cell)
{
    int maskfd;
    int rasrow;
    int row;
    register unsigned char *ras;
    register CELL *c;
    register int col;

    if (!graphics.dirty)
	return 0;

    maskfd = G_maskfd () ;
    if (maskfd < 0)
	    return 0;

    rasrow = prows_sp;
    for (row = graphics.window.top; row <= graphics.window.bottom-grows-1; row++)
    {
	int i;
	G_get_map_row (maskfd, c = cell, row);

	col = G_window_cols();
	ras = graphics.raster[rasrow++];
	for (i=0; i<pcols_sp;  i++)
		ras++;
	while (col-- > 0)
	{
	    if (*c++ == 0)
		*ras = 0;
	    ras++;
	}
    }

    return 0;
}
