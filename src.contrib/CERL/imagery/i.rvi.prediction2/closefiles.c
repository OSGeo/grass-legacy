#include "gis.h"
#include "globals.h"

/* This routine closes up the cell maps, frees up the row buffers and
   use a less than perfect way of setting the color maps for the output
   to grey scale.  */

closefiles(rowbuf)
CELL *rowbuf[NBANDS];
{
	int band;
	struct Colors colors;
	struct Range range ;
	CELL min, max ;

	/* make a real component color table */
		G_close_cell(fd_output);
		G_read_range(outputfiles, G_mapset(), &range) ;
		G_get_range_min_max (&range, &min, &max);
		G_make_grey_scale_colors(&colors, min, max) ;
	for (band=0; band<NBANDS; band++)
		free(rowbuf[band]);
}
