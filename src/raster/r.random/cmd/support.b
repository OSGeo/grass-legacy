#include "gis.h"

execute_random (targets, count, input, mapset, cat_zero, raster, sites, verbose)
    long targets, count;
    char *input, *mapset, *raster, *sites;
    CELL cat_zero;
{
    char buf[200];
    char *mapset;
    char *input_cell, *output_cell, *sitefile;
    int nt, targets;
    long nc, count;
    int   number;
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
    char msg[100];

    CELL *cell;

    struct Option *opt1, *opt2, *opt3, *opt4, *opt5 ;
    struct Flag *flag1, *flag2 ;

    opt1 = G_define_option() ;
    opt1->key        = "input" ;
    opt1->type       = TYPE_STRING ;
    opt1->required   = YES ;
    opt1->gisprompt  = "old,cell,raster" ;
    opt1->description= "Name of existing raster map" ;

    opt2 = G_define_option() ;
    opt2->key        = "output" ;
    opt2->type       = TYPE_STRING ;
    opt2->required   = YES;
    opt2->gisprompt  = "new,cell,raster" ;
    opt2->description= "Name of the output raster map" ;

    opt3 = G_define_option() ;
    opt3->key        = "number" ;
    opt3->type       = TYPE_INTEGER ;
    opt3->answer     = "0" ;
    opt3->required   = NO ;
    opt3->description= "The number of the map's size" ;

    opt4 = G_define_option() ;
    opt4->key        = "percent" ;
    opt4->type       = TYPE_DOUBLE ;
    opt4->required   = NO ;
    opt4->answer     = "0" ;
    opt4->options    = "0-100" ;
    opt4->description= "The percentage of the map's size" ;

    opt5 = G_define_option() ;
    opt5->key        = "site" ;
    opt5->type       = TYPE_STRING ;
    opt5->required   = NO ;
    opt5->description= "The sitefile the user want to generate" ;

    flag1 = G_define_flag() ;
    flag1->key         = 'q' ;
    flag1->description = "Quiet" ;

    flag2 = G_define_flag() ;
    flag2->key         = 'z' ;
    flag2->description = "Generate random locations against category zero";

    G_gisinit (argv[0]);

    if (G_parser(argc, argv))
	    exit(-1);

    verbose = (!flag1->answer);

    cat_zero = flag2->answer;

    input_cell = opt1->answer;

    output_cell = opt2->answer;

    sitefile = opt4->answer;


    /* look for n[%] */
    percent = 0;
    number  = 0;

    number = strcmp("0", opt3->answer) ;

    percent = strcmp("0", opt4->answer) ;

    if ( percent == 0 && number == 0)
	    usage(NULL, "You must specify %(percentage), if no n(number) specified");

    if (percent != 0 && number != 0)
	    usage(NULL,"You can specify only one of the n(number) or %(percentage)");

    if (percent)
    {
	    if(sscanf (opt4->answer, "%lf", &percentage) != 1
		|| percentage <= 0.0 || percentage > 100.0)
		    sprintf(msg, "%f: <%f> illegal percent specification",opt4->answer);
    }
    if (number)
    {
	    if (sscanf (opt3->answer, "%d", &targets) != 1
		|| targets <= 0)
		    sprintf (msg, "%d: <%d> illegal count specification",opt3->answer);
    }

    G_get_window (&window);

    /* check GRASS datafile access, legality */

    if (sitefile == NULL && output_cell == NULL)
	    usage (NULL, "must specify sitefile, if no cell file specified");

    mapset = G_find_cell2 (input_cell, "");
    if (mapset == NULL)
    {
	    sprintf (buf, "%s: <%s> cell file not found", G_program_name(),
		input_cell);
	    G_fatal_error (buf);
	    exit(1);
    }
    if (output_cell != NULL && G_legal_filename (output_cell) < 0)
    {
	    sprintf (buf, "%s: <%s> illegal cell file name", G_program_name(),
		output_cell);
	    G_fatal_error (buf);
	    exit(1);
    }
    if (sitefile != NULL && G_legal_filename (sitefile) < 0)
    {
	    sprintf (buf, "%s: <%s> illegal sites file name", G_program_name(),
		sitefile);
	    G_fatal_error (buf);
	    exit(1);
    }

    /* get some support files for the input layer */
    if(cats_ok = (G_read_cats (input_cell, mapset, &cats) >= 0))
    {
	    sprintf (buf, "Random sites on [%s in %s]", input_cell, mapset);
	    G_set_cats_title (buf, &cats);
    }

/*
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
*/

    nrows = G_window_rows();
    ncols = G_window_cols();

    cell = G_allocate_cell_buf();

    /* open the data files */
    infd = G_open_cell_old (input_cell, mapset);
    if (infd < 0)
    {
	    sprintf (buf, "%s: unable to open cell file [%s]", G_program_name(),
		input_cell);
	    G_fatal_error (buf);
	    exit(1);
    }
    if (output_cell != NULL)
    {
	    outfd = G_open_cell_new (output_cell);
	    if (outfd < 0)
	    {
		    sprintf (buf, "%s: unable to create cell file [%s]", 
			G_program_name(), output_cell);
		    G_fatal_error (buf);
		    exit(1);
	    }
    }
    if (sitefile)
    {
	    sitefd = G_fopen_sites_new (sitefile);
	    if (sitefd == NULL)
	    {
		    sprintf (buf, "%s: unable to create site file [%s]",
			G_program_name() , sitefile);
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
			    sprintf (buf, "%s: can't read cell file [%s]",
				G_program_name(), input_cell);
			    G_fatal_error (buf);
			    exit(1);
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
	    x = count * percentage / 100.0 +.5;
	    targets = x;
	    if (x != targets)
		    G_fatal_error ("Too many random locations for this algorithm");
    }
    else if (targets > count)
    {
	    sprintf (buf,
		"%s: There aren't %ld %scells in the current window",
		G_program_name(), targets, cat_zero?"":"non-zero ");
	    G_fatal_error (buf);
	    exit(1);
    }
    if (targets <= 0)
    {
	    sprintf (buf,
		"%s: There aren't any valid locations in the current window",
		G_program_name());
	    G_fatal_error (buf);
	    exit(1);
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
		    sprintf (buf, "%s: can't read cell file [%s]",
			G_program_name(), input_cell);
		    G_fatal_error (buf);
		    exit(1);
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
/*
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
*/
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
			    sprintf (hist.datsrc_2, "Random points over %f of the base map",
				opt4->answer);
		    if (number)
			    sprintf (hist.datsrc_2, "%d random points on the base map",
				opt3->answer);
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
	G_usage() ;
	exit(1);
}

contrast (color)
{
	return color > 127 ? color-127 : color+127;
}
