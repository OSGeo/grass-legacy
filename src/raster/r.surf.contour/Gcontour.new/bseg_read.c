#include "gis.h"
#include "cseg.h"

static char *me="bseg_read_cell";

bseg_read_cell (bseg, map_name, mapset)
	BSEG	*bseg;
	char	*map_name, *mapset;
{
	int	row, nrows;
	int	col, ncols;
	int	map_fd;
	char	msg[100];
	CELL	*buffer;

	bseg->name   = NULL;
	bseg->mapset = NULL;

	map_fd = G_open_cell_old (map_name, mapset);
	if (map_fd < 0)
	{
		sprintf (msg, "%s(): unable to open file [%s] in [%s], %d", 
			me, map_name, mapset, map_fd);
		G_warning (msg);
		return -3;
	}
	nrows = G_window_rows ();
	ncols = G_window_cols ();
	buffer = G_allocate_cell_buf ();
	for (row=0; row < nrows; row++)
	{
		if (G_get_map_row (map_fd, buffer, row) < 0)
		{
			free (buffer);
			G_close_cell (map_fd);
			sprintf (msg, "%s(): unable to read file [%s] in [%s], %d %d",
				me, map_name, mapset, row, nrows);
			G_warning (msg);
			return -2;
		}
		for (col = ncols; col >= 0; col--)
		{
			bseg_put (bseg, &(buffer[col]), row, col);
		}
	}

	G_close_cell (map_fd);
	free (buffer);

	bseg->name = G_store (map_name);
	bseg->mapset = G_store (mapset);

	return 0;
}
