#include "gis.h"

execute_random (targets, count, input, mapset, cat_zero, raster, sites, verbose)
long targets, count;
char *input, *mapset, *raster, *sites;
CELL cat_zero;
{
	long nt;
	long nc;
	double east, north;
	struct Cell_head window;
	int nrows, ncols, row, col;
	int infd, outfd;
	FILE *sitefd;
	char msg[256];
	CELL *cell;
	long make_rand();
	void init_rand();

	G_get_window (&window);

	nrows = G_window_rows();
	ncols = G_window_cols();
	cell = G_allocate_cell_buf();

	/* open the data files */
	infd = G_open_cell_old (input, mapset);
	if (infd < 0)
	{
		sprintf (msg, "%s: unable to open raster file [%s]", G_program_name(), input);
		G_fatal_error (msg);
		exit(1);
	}
	if (raster != NULL)
	{
		outfd = G_open_cell_new (raster);
		if (outfd < 0)
		{
			sprintf (msg, "%s: unable to create raster file [%s]", 
			    G_program_name(), raster);
			G_fatal_error (msg);
			exit(1);
		}
	}
	if (sites)
	{
		sitefd = G_fopen_sites_new (sites);
		if (sitefd == NULL)
		{
			sprintf (msg, "%s: unable to create site file [%s]",
			    G_program_name() , sites);
			G_fatal_error (msg);
			exit(1);
		}
		else
			fprintf (sitefd, "desc|Random sites from [%s in %s]\n", input, mapset);
	}

	if (verbose)
	{
		fprintf (stderr, "Writing ");
		if (raster)
			fprintf (stderr, "raster file [%s] ", raster);
		if (sites && raster)
			fprintf (stderr, "and ");
		if (sites)
			fprintf (stderr, "site file [%s] ", sites);
		fprintf (stderr, "... ");
		G_percent (0, targets, 2);
	}

	init_rand();
	nc = count ;
	nt = targets;
	for (row = 0; row < nrows && nt ; row++)
	{
		if (G_get_map_row (infd, cell, row) < 0)
		{
			sprintf (msg, "%s: can't read raster file [%s]",
			    G_program_name(), input);
			G_fatal_error (msg);
			exit(1);
		}

		for (col = 0; col < ncols && nt ; col++)
		{
			if (cat_zero == 0 && cell[col] == 0)
				continue;

			if (make_rand() % nc < nt)
			{
				nt--;
				if (cell[col] == 0)
					cell[col] = cat_zero;

				if (sites)
				{
					north = window.north - (row + .5) * window.ns_res;
					east  = window.west  + (col + .5) * window.ew_res;
					sprintf (msg, "#%ld", (long) cell[col]);
					G_put_site (sitefd, east, north, msg);
				}
				G_percent ((targets -nt), targets, 2);
			}
			else
				cell[col] = 0;

			nc-- ;
		}

		while (col < ncols)
			cell[col++] = 0;

		if (raster)
			G_put_map_row (outfd, cell, row);
	}

	/* close files */
	G_close_cell(infd);
	if (sites)
		fclose (sitefd);
	if (raster)
		G_close_cell(outfd);
}
