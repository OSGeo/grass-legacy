#include <stdlib.h>
#include <grass/gis.h>
#include "globals.h"

/* This routine closes up the cell maps, frees up the row buffers and
   use a less than perfect way of setting the color maps for the output
   to grey scale.  */

int closefiles(CELL *rowbuf[NBANDS])
{
	int band;
	struct Colors colors;
	struct Range range ;
	CELL min, max ;

	/* make a real component color table */
	for (band=0; band<NBANDS; band++)
	{
		G_close_cell(fd_output[band]);
		G_free (rowbuf[band]);
		G_read_range(outputfiles[band], G_mapset(), &range) ;
		G_get_range_min_max (&range, &min, &max);
		G_make_grey_scale_colors(&colors, min, max) ;
		G_write_colors(outputfiles[band], G_mapset(), &colors) ;
	}

	return 0;
}
