#include "watershed.h"

int valid_basins (char *accum_name, OUTPUT *output)
{
	int	i, r, c, fd;
	/* int nrows, ncols; */
	CELL	*buf;
	B_FACTS	*basin_facts, *basin, *down_basin;
	char	*mapset;
	struct Cell_head *window;

	window = &(output->window);
	/*
	nrows = G_window_rows ();
	ncols = G_window_cols ();
	*/
	if (NULL == (mapset = G_find_cell (accum_name, ""))) {
		free_output (output);
		G_fatal_error ("accum file missing in valid_basins()");
		exit (1);
	}
	if (-1 == (fd = G_open_cell_old (accum_name, mapset))) {
		free_output (output);
		G_fatal_error ("unable to open accum file in valid_basins()");
		exit (1);
	}
	buf = G_allocate_cell_buf ();
	basin_facts = output->basin_facts;
	for (i = output->num_basins - 1; i >= 0 ; i--) {
		basin = &(basin_facts[i]);
		if (basin->valid == 0) {
			if (basin->down_basin >= 0)
				basin_facts[basin->down_basin].valid = 0;
		} else {
			if (basin->down_basin >= 0)
				down_basin = &(basin_facts[basin->down_basin]);
			else	down_basin = NULL;
			r = (int) (window->north - basin->northing) / window->ns_res - 1;
			c = (int) (basin->easting - window->west) / window->ew_res;
			/*
			if (r < 0 || r >= nrows || c < 0 || c>= ncols) {
				fprintf (stderr, "r:%d c:%d big error\n", r,c);
				exit (-1);
			}
			*/
			G_get_c_raster_row (fd, buf, r);
			if (buf[c] < 0)	{
				basin->valid = 0;
				if (down_basin != NULL)
					down_basin->valid = 0;
			}
		}
	}
	G_free (buf);

	return 0;
}
