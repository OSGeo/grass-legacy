#include "gis.h"
#include "cseg.h"

static char *me = "bseg_write_cell";

bseg_write_cellfile (bseg, map_name)
	BSEG	*bseg;
	char	*map_name;
{
	int	map_fd;
	int	row, nrows;
	int	col, ncols;
	char	msg[100];
	CELL	*buffer;
	CELL	value;

	map_fd = G_open_cell_new (map_name);
	if (map_fd < 0)
	{
		sprintf (msg,"%s(): unable to open new map layer [%s]", 
			me, map_name);
		G_warning (msg);
		return -1;
	}
	nrows = G_window_rows ();
	ncols = G_window_cols ();
	buffer = G_allocate_cell_buf ();
	for (row=0; row<nrows; row++)
	{
		for (col = 0; col < ncols; col++)
		{
			bseg_get (bseg, &value, row, col);
			buffer[col] = value;
		}
		if (G_put_map_row (map_fd, buffer, row) < 0)
		{
			free (buffer);
			G_unopen_cell (map_fd);
			sprintf (msg,
			    "%s(): unable to write new map layer [%s], row %d", 
				me, map_name, row);
			G_warning (msg);
			return -2;
		}
	}
	free (buffer);
	G_close_cell (map_fd);
	return 0;
}
