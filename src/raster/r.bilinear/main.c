#include "gis.h"
#include "bilinear.h"
#include <stdio.h>

main(argc, argv)
int   argc;
char *argv[];
{
	struct History hist;
	struct Colors colr;
	struct Range range;
	CELL *ncell, *scell, *outcell, *mask;
	int done;
	int	i, j;	


	G_gisinit(argv[0]);

    if (!getargs(argc, argv)) {
		G_usage();
	    exit(1);
		}

/* open output file first, using current active region settings */

	outfd = G_open_cell_new (result);
	if (outfd < 0)
		exit(1);
	outcell = G_allocate_cell_buf();
	out_rows = G_window_rows();
	out_cols = G_window_cols();
	G_get_window (&out_region);

/*  if mask is active, create buffer, then use mask to constrain output */

	mask = ((maskfd = G_maskfd()) >= 0) ? G_allocate_cell_buf() : NULL;
		
/* reset active region before opening input mapset to control row read */

	if (G_get_cellhd (name, mapset, &in_region) < 0)
		exit(1);
	G_set_window (&in_region);
	infd = G_open_cell_old (name, mapset);
	if (infd < 0)
		exit(1);

	ncell = G_allocate_cell_buf();
	scell = G_allocate_cell_buf();
	in_rows = G_window_rows();
	in_cols = G_window_cols();

/* compare input and output regions, defining "no data" columns west and east */

	if (out_of_range (out_region.west + (0.5 * out_region.ew_res), WEST))
		null_columns (WEST);
	if (out_of_range (out_region.east - (0.5 * out_region.ew_res), EAST))
		null_columns (EAST);

/* create arrays of eastings used in interpolations */

	make_easting_lookup_arrays ();

/* calculate distance fractions re-used in interpolation formula iterations */

	set_fractions (in_region.cols, out_region.cols);
	
/* initialize northing counters, load one row of input
	if null rows at N boundary, write to output here
*/

	pre_bilinear_initialization (scell, outcell);


	if (verbose)
		fprintf (stderr, "percent complete: ");

	while (out_north > in_region.south + (in_region.ns_res * 0.5)) {
		load_rows (&ncell, &scell);
		if (done = bilinear (ncell, scell, outcell, mask))
			break;
		if (verbose)
			G_percent (out_row, out_rows, 2);
		}

	if (!done) {
		fill_south (ncell, scell, outcell, mask);
		}

	if (verbose)
		G_percent (out_row, out_rows, 2);

	G_close_cell (infd);

	if (verbose)
		printf ("\nCreating support files for %s\n", result);

	G_close_cell (outfd);

	if (G_read_colors (name, mapset, &colr) > 0)
	{
		if(G_read_range (result, G_mapset(), &range) > 0)
		{
		    CELL min, max, cmin, cmax;
		    G_get_range_min_max (&range, &min, &max);
		    G_get_color_range (&cmin, &cmax, &colr);
		    if (min > cmin) cmin = min;
		    if (max < cmax) cmax = max;
		    G_set_color_range (cmin, cmax, &colr);
		}
		G_write_colors (result, G_mapset(), &colr);
	}

	if (G_read_history (result, G_mapset(), &hist) >= 0) {
		strcpy (hist.edhist[j = 0], "\0");
		for (i = 0; i < argc; i++) {
			if (strlen (hist.edhist[j]) + strlen (argv[i]) > RECORD_LEN - 1)
				strcpy (hist.edhist[j++], "\0");
			strcat (hist.edhist[j], argv[i]);
			strcat (hist.edhist[j], " ");
			}
		hist.edlinecnt = j + 1;
		G_write_history (result, &hist); 
		}

	exit(0);
}
