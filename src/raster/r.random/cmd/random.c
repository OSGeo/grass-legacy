#include "gis.h"
#include "local_proto.h"

int execute_random (long targets, long count, char *input, char *mapset, CELL cat_zero, char *raster, char *sites, int verbose)
{
	long nt;
	long nc;
	struct Cell_head window;
	int nrows, ncols, row, col;
	int infd, outfd;
	FILE *sitefd;
	char msg[256];
	CELL *cell;
        Site *mysite;
        Site_head site_info;

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
		mysite=G_site_new_struct(-1,2,0,1);
		sitefd = G_fopen_sites_new (sites);
		if (sitefd == NULL)
		{
			sprintf (msg, "%s: unable to create site file [%s]",
			    G_program_name() , sites);
			G_fatal_error (msg);
			exit(1);
		}
		else
                {
                  	site_info.form=NULL;
                  	site_info.labels=NULL;
                  	site_info.time=NULL;
                  	site_info.stime=NULL;
                  	site_info.name=(char *)G_malloc(80*sizeof(char));
                  	site_info.desc=(char *)G_malloc(80*sizeof(char));
                  	if (site_info.desc==NULL || site_info.name==NULL)
                          G_fatal_error("memory allocation error");
			sprintf (site_info.desc, 
                                 "Random sites from [%s in %s]", 
                                 input, mapset);
			sprintf (site_info.name, "%s", sites);
			G_site_put_head (sitefd, &site_info);
		}
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
					mysite->north = window.north - (row + .5) * window.ns_res;
					mysite->east  = window.west  + (col + .5) * window.ew_res;
					mysite->dbl_att[0]=(double) cell[col];
					G_site_put (sitefd, mysite);
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
			G_put_map_row (outfd, cell);
	}

	/* close files */
	G_close_cell(infd);
	if (sites)
		fclose (sitefd);
	if (raster)
		G_close_cell(outfd);

    return 0;
}
