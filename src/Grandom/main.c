/*********************************************************************
 * GRASS program to create a cell file with non-zero data
 *         in random locations.
 * Locations will be within non-zero data in the input layer.
 *        (unless -z option specified)
 * If it necessary to restrict the locations to only some categories
 *    of the input map, a reclass map should be constructed first
 *    and used as input to this program.
 *
 * Number of random cells can be a fixed number or a percentage of
 *         cells.
 *
 * Resulting cell file consists of original cell values at the selected
 *         random locations and zero elsewhere.
 *
 * A sitefile may optionally be created.
 *
 * Usage: Grandom [-vz] inputcell outputcell n[%] [sitefile]
 *
 *        -v means no-verbose
 *        -z means generate random locations for zero-data as well
 *
 *        -  for outputcell means no cell file (but must specify sitefile)
 *
 * Note if called by random, the program name will actually be -#
 *      where # is some integer. The # is the number of cells in
 *      the inputmap. The next argument will be argv[0], etc.
 *********************************************************************/
#include "gis.h"

static char *me;
static int verbose = 1;
static CELL cat_zero = 0;

main (argc, argv) char *argv[];
{
    char buf[200];
    char *mapset;
    char input_cell[40], *output_cell, *sitefile;
    char *NPOINTS;
    int nt, targets;
    long nc, count;
    short percent;
    double percentage;
    double east, north;
    struct Cell_head window;
    int nrows, ncols, row, col;
    int *rand;
    char *c;
    struct History hist;
    struct Colors colr;
    struct Categories cats;
    int cats_ok, colr_ok;
    int infd, outfd;
    FILE *sitefd;
    int red, grn, blu;

    CELL *cell;

/* check calling parameters */
    if (sscanf (argv[0], "-%ld", &count) == 1) /* are we random? */
    {
	argv++;
	argc--;
	if (count <= 0)
	    count = 0;	/* hopefully this wont happen */
    }
    else
	count = 0;

    me = argv[0];
    if (argc <= 1)
	usage (NULL,NULL);
    while (argc > 1 && argv[1][0] == '-')
    {
	char *option;
	option = &argv[1][1];
	if (*option == 0) option--;
	while (*option)
	{
	    switch (*option++)
	    {
	    case 'v': verbose  = 0; break;
	    case 'z': cat_zero = 1; break;
	    default: usage (argv[1], "Unregcognized option(s)");
	    }
	}
	argc--;
	argv++;
    }

    if (argc < 4 || argc > 5)
	usage (me,argc<4?"not enough args":"too many args");

/* look for n[%] */
    NPOINTS = argv[3];
    percent = 0;
    for (c = NPOINTS; *c; c++)
	if (*c == '%')
	    percent = 1;
    if (percent)
    {
	if(sscanf (NPOINTS, "%lf", &percentage) != 1
	|| percentage <= 0.0 || percentage > 100.0)
	    usage (NPOINTS,"illegal percent specification");
    }
    else
    {
	if (sscanf (NPOINTS, "%d", &targets) != 1
	|| targets <= 0)
	    usage (NPOINTS,"illegal count specification");
    }

    G_gisinit (me);
    G_get_window (&window);

/* check GRASS datafile access, legality */
    strcpy (input_cell, argv[1]);
    if (strcmp(argv[2],"-") == 0)
	output_cell = NULL;
    else
	output_cell = argv[2];
    sitefile = argc>4 ? argv[4] : NULL;

    if (sitefile == NULL && output_cell == NULL)
	usage (NULL, "must specify sitefile, if no cell file specified");

    mapset = G_find_cell2 (input_cell, "");
    if (mapset == NULL)
    {
	sprintf (buf, "%s: [%s] cell file not found", me, input_cell);
	G_fatal_error (buf);
    }
    if (output_cell != NULL && G_legal_filename (output_cell) < 0)
    {
	sprintf (buf, "%s: [%s] illegal cell file name", me, output_cell);
	G_fatal_error (buf);
    }
    if (sitefile != NULL && G_legal_filename (sitefile) < 0)
    {
	sprintf (buf, "%s: [%s] illegal sites file name", me, sitefile);
	G_fatal_error (buf);
    }

/* get some support files for the input layer */
    if(cats_ok = (G_read_cats (input_cell, mapset, &cats) >= 0))
    {
	sprintf (buf, "Random sites on [%s in %s]", input_cell, mapset);
	G_set_cats_title (buf, &cats);
    }
    if(colr_ok = (G_read_colors (input_cell, mapset, &colr) >= 0))
    {
	CELL cat;
	for (cat = colr.min; cat <= colr.max; cat++)
	{
	    G_get_color (cat, &red, &grn, &blu, &colr);
	    red = contrast (red);
	    grn = contrast (grn);
	    blu = contrast (blu);
	    G_set_color (cat, red, grn, blu, &colr);
	}
    }

    nrows = G_window_rows();
    ncols = G_window_cols();

    cell = G_allocate_cell_buf();

/* open the data files */
    infd = G_open_cell_old (input_cell, mapset); 
    if (infd < 0)
    {
	sprintf (buf, "%s: unable to open cell file [%s]", me, input_cell);
	G_fatal_error (buf);
    }
    if (output_cell != NULL)
    {
	outfd = G_open_cell_new (output_cell);
	if (outfd < 0)
	{
	    sprintf (buf, "%s: unable to create cell file [%s]", me, output_cell);
	    G_fatal_error (buf);
	}
    }
    if (sitefile)
    {
	sitefd = G_fopen_sites_new (sitefile);
	if (sitefd == NULL)
	{
	    sprintf (buf, "%s: unable to create site file [%s]", me, sitefile);
	    G_warning (buf);
	    sitefile = NULL;
	}
	else
	    fprintf (sitefd, "desc|Random sites from [%s in %s]\n",
		input_cell, mapset);
    }

/* if we haven't been told how many cells are in the map, find out */
    if (count <= 0 || cat_zero)
    {
	count = 0;
	if (verbose)
	    fprintf (stderr, "Reading [%s in %s] ... ", input_cell, mapset);
	for (row = 0; row < nrows; row++)
	{
	    if (verbose)
		G_percent (row, nrows, 10);
	    if (G_get_map_row (infd, cell, row) < 0)
	    {
		sprintf (buf, "%s: can't read cell file [%s]", me, input_cell);
		G_fatal_error (buf);
	    }
	    col = ncols;
	    if (cat_zero)
		while (col-- > 0)
		{
		    if (*cell >= cat_zero)
			cat_zero = *cell + 1;
		    count++;
		    cell++;
		}
	    else
		while (col-- > 0)
		    if (*cell++)
			count++;
	    cell -= ncols;
	}
	if (verbose)
	    G_percent (nrows, nrows, 10);
    }
    if (verbose)
	    fprintf (stderr, "%ld data cells\n", count);

    if (percent)
    {
	long x;
	x = count * percentage / 100.0;
	targets = x;
	if (x != targets)
	    G_fatal_error ("Too many random locations for this algorithm");
    }
    else if (targets > count)
    {
	sprintf (buf,
	    "%s: There aren't %ld %scells in the current window",
	    me, targets, cat_zero?"":"non-zero ");
	G_fatal_error (buf);
    }
    if (targets <= 0)
    {
	sprintf (buf,
	    "%s: There aren't any valid locations in the current window", me);
	G_fatal_error (buf);
    }

    if (verbose)
	fprintf (stderr, "Creating list of %d random cells\n", targets);

    rand = (int *) G_calloc (targets+2, sizeof(int));
    create_rand (rand, targets, count);

    if (verbose)
    {
	fprintf (stderr, "Writing ");
	if (output_cell)
	    fprintf (stderr, "cell file [%s] ", output_cell);
	if (sitefile && output_cell)
		fprintf (stderr, "and ");
	if (sitefile)
	    fprintf (stderr, "sitefile [%s] ", sitefile);
	fprintf (stderr, "... ");
	G_percent (0, targets, 10);
    }

    nc = nt = 0;
    for (row = 0; row < nrows && nc < count && nt < targets; row++)
    {
	if (G_get_map_row (infd, cell, row) < 0)
	{
	    sprintf (buf, "%s: can't read cell file [%s]", me, input_cell);
	    G_fatal_error (buf);
	}

	for (col = 0; col < ncols && nc < count && nt < targets; col++)
	{
	    if (cat_zero == 0 && cell[col] == 0)
		continue;
	    nc++;
	    if (rand[nt] == nc)
	    {
		nt++;
		if (cell[col] == 0)
		    cell[col] = cat_zero;
		if (sitefile)
		{
		    north = window.north - (row + .5) * window.ns_res;
		    east  = window.west  + (col + .5) * window.ew_res;
		    fprintf (sitefd, "%.0lf|%.0lf|#%ld\n",
			east, north, (long) cell[col]);
		}
		G_percent (nt, targets, 10);
	    }
	    else
		cell[col] = 0;
	}
	while (col < ncols)
		cell[col++] = 0;

	if (output_cell != NULL)
	    G_put_map_row (outfd, cell, row);
    }

/* close files and create support files */
    if (sitefile)
	fclose (sitefd);
    G_close_cell(infd);
    if (output_cell != NULL)
    {
	if (verbose)
	    fprintf (stderr, "Creating support files\n");
	G_close_cell(outfd);
	if (colr_ok)
	{
	    if (cat_zero)
	    {
		G_get_color ((CELL) 0, &red, &grn, &blu, &colr);
		red = contrast (red);
		grn = contrast (grn);
		blu = contrast (blu);
		G_set_color (cat_zero, red, grn, blu, &colr);
	    }
	    G_write_colors (output_cell, G_mapset(), &colr);
	}
	if (cats_ok)
	{
	    if (cat_zero)
		G_set_cat (cat_zero, "Original data was 0 for these sites", &cats);
	    G_write_cats (output_cell, &cats);
	}
	if (G_read_history (output_cell, G_mapset(), &hist) >= 0)
	{
	    sprintf (hist.datsrc_1, "Based on map [%s in %s]",
		    input_cell, mapset);
	    if (percent)
		sprintf (hist.datsrc_2, "Random points over %s of the base map",
		    NPOINTS);
	    else
		sprintf (hist.datsrc_2, "%s random points on the base map",
		    NPOINTS);
	    G_write_history (output_cell, &hist);
	}
    }
    exit(0);
}

usage (msg1, msg2)
{
    if (msg1)
	fprintf (stderr, "%s: ", msg1);
    if (msg2)
	fprintf (stderr, "%s\n", msg2);
    fprintf (stderr, "Usage: %s [-vz] inputcell outputcell n[%%] [sitefile]\n",
	me);
    exit(1);
}

contrast (color)
{
    return color > 127 ? color-127 : color+127;
}
