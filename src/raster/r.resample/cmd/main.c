#include "gis.h"
#include <stdio.h>

main(argc, argv)
int   argc;
char *argv[];
{
	struct History hist;
	struct Categories cats;
	struct Colors colr;
	struct Cell_head cellhd;
	int hist_ok, colr_ok, cats_ok;
	char name[100], *mapset;
	char result[100];
	CELL *cell;
	int nrows, ncols;
	int row;
	int infd, outfd;
	int verbose;
	struct Flag *flag1 ;
	struct Option *opt1 ;
	struct Option *opt2 ;


	/* Define the different options */

	opt1 = G_define_option() ;
	opt1->key        = "input";
	opt1->type       = TYPE_STRING;
	opt1->required   = YES;
	opt1->gisprompt  = "old,cell,raster" ;
	opt1->description= "Name of an input layer" ;

	opt2 = G_define_option() ;
	opt2->key        = "output";
	opt2->type       = TYPE_STRING;
	opt2->required   = YES;
	opt2->gisprompt  = "new,cell,raster" ;
	opt2->description= "Name of an output layer";

	/* Define the different flags */

	flag1 = G_define_flag() ;
	flag1->key         = 'q' ;
	flag1->description = "Quiet" ;

	G_gisinit(argv[0]);

	if (G_parser(argc, argv))
		exit (-1);

	verbose = (! flag1->answer);

	strcpy (name, opt1->answer);
	strcpy (result, opt2->answer);
	mapset = G_find_cell2 (name, "");

	if (mapset == NULL)
	{
		char buf[200];
		sprintf (buf, "cell file [%s] not found", name);
		G_fatal_error (buf);
	}

	if (G_legal_filename (result) < 0)
	{
		char buf[200];

		sprintf (buf, "[%s] illegal name", result);
		G_fatal_error (buf);
	}

	hist_ok = G_read_history (name, mapset, &hist) >= 0;
	colr_ok = G_read_colors (name, mapset, &colr) > 0;
	cats_ok = G_read_cats (name, mapset, &cats) >= 0;

	infd = G_open_cell_old (name, mapset);
	if (infd < 0)
		exit(1);

	if (G_get_cellhd (name, mapset, &cellhd) < 0)
		exit(1);

	cell = G_allocate_cell_buf();
	nrows = G_window_rows();
	ncols = G_window_cols();

	outfd = G_open_cell_new (result);

	if (outfd < 0)
		exit(1);

	if (verbose)
		printf ("percent complete: ");

	for (row = 0; row < nrows; row++)
	{
		if (verbose)
			percent (row, nrows, 10);
		if (G_get_map_row (infd, cell, row) < 0)
			exit(1);
		if (G_put_map_row (outfd, cell) < 0)
			exit(1);
	}

	if (verbose)
		percent (row, nrows, 10);

	G_close_cell (infd);

	if (verbose)
		printf ("Creating support files for %s\n", result);

	G_close_cell (outfd);

	if (cats_ok)
		G_write_cats (result, &cats);

	if (colr_ok)
		G_write_colors (result, G_mapset(), &colr);

	if (hist_ok)
		G_write_history (result, &hist);

	exit(0);
}
