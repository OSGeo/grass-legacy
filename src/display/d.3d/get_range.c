#include <gis.h>
#include "options.h"

get_range(min, max)
	CELL *min, *max ;
{
	int fd ;
	CELL *array ;
	CELL *ptr ;
	int i, j ;
	int first;
	array  = G_allocate_cell_buf() ;

	if ((fd = G_open_cell_old(elevfile, elevfile_mapset)) == -1) 
	{
		char buffer[256] ;
		sprintf(buffer, "Raster file [%s] in [%s] not available",
			elevfile, elevfile_mapset) ;
		G_fatal_error(buffer) ;
	}

	fprintf(stderr, "Finding min/max elevation ...");
	first = 1;
	for(i=0; i<window.rows; i++)
	{
		G_percent (i, window.rows, 2);
		G_get_map_row_nomask (fd, array, i) ;
		for(j=window.cols, ptr=array; j; j--, ptr++)
		{
			if (*ptr == 0 && ! do_zero )
				continue ;
			if (first)
			{
			    *min = *max = *ptr;
			    first = 0;
			}
			if (*ptr < *min) *min = *ptr ;
			if (*ptr > *max) *max = *ptr ;
		}
	}
	G_percent (i, window.rows, 2);
	G_close_cell(fd);
	free(array) ;
}
