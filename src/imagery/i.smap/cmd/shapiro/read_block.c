#include "imagery.h"
#include "files.h"
#include "../bouman/region.h"

read_block (img, region, files)
    CELL ***img;           /* img[band][row[col] */
    struct Region *region;
    struct files *files;
{
    int band, row, col;

    for (band = 0; band < files->nbands; band++)
    {
	for (row = region->ymin; row < region->ymax; row++)
	{
	    if(G_get_map_row (files->band_fd[band], files->cellbuf, row) < 0)
		exit(1);
	    for (col = region->xmin; col < region->xmax; col++)
		    img[band][row][col] = files->cellbuf[col];
	}
    }
}
