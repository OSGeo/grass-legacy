/* @(#)main.c	1.3 2/27/91 */
#include "gis.h"
#include <stdio.h>

main(argc, argv) char *argv[];
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
	int row, col;
	int infd;
	FILE *outfd;
	int verbose;
	char *me;

	short start[1000], stop[1000], buf1, buf2;
	int runcnt, runpos;
	int onrun, runstart, runstop, x;

	G_gisinit (me = argv[0]);

	verbose = 1;
	if (argc < 2)
		usage (me);
	if (argv[1][0] == '-')
	{
		if (strcmp (argv[1], "-v") == 0)
			verbose = 0;
		else
		{
			fprintf (stderr, "%s: %s  illegal option\n", me, argv[1]);
			usage (me);
		}
		argc--;
		argv++;
	}
	if (argc < 3)
		usage(me);
	strcpy (name, argv[1]);
	strcpy (result, argv[2]);
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

	infd = G_open_cell_old (name, mapset);
	if (infd < 0)
		exit(1);
	if (G_get_cellhd (name, mapset, &cellhd) < 0)
		exit(1);
	cell = G_allocate_cell_buf();
	nrows = G_window_rows();
	ncols = G_window_cols();

	if((outfd = fopen(result,"w+")) == NULL){
		fprintf(stderr, "Cant create %s\n",result);
		exit(1);
	}

	buf1 = (short)nrows;
	swab(&buf1, &buf2, 2);
	fwrite(&buf2, sizeof(short), 1, outfd); /* # of rows */
	buf1 = (short)ncols;
	swab(&buf1, &buf2, 2);
	fwrite(&buf2, sizeof(short), 1, outfd); /* # of cols */

	if (verbose)
		printf ("percent complete: ");
	for (row = 0; row < nrows; row++)
	{
		if (verbose)
			percent (row, nrows, 10);
		if (G_get_map_row (infd, cell, row) < 0)
			exit(1);
		runcnt = 0; /* set run count back to 0 */
		runpos = 1; /* set run position in buffer to 1 */
		onrun = 0;
		for (x = 1, col = ncols; x <= ncols; x++, col--){
			if ((cell[col] != 0) && (onrun != 1)){
				onrun = 1;
				runcnt++;
				start[runcnt] = (short)x;
		/*		printf("on%d",start[runcnt]);*/
			}
			if ((cell[col] == 0) && (onrun == 1)){
				onrun = 0;
				stop[runcnt] = (short)x;
/*				printf("off%d",stop[runcnt]);*/
			}
		}
/*		printf("\n");*/
		if (runcnt == 0){
			buf1 = 0;
			fwrite(&buf1, sizeof(short), 1, outfd);
		} else{
			buf1 = (short)runcnt;
			swab(&buf1, &buf2, 2);
			fwrite(&buf2, sizeof(short), 1, outfd);
			for (x = 1; x <= runcnt ; x++){
				swab(&start[x], &buf1, 2);
				fwrite(&buf1, sizeof(short), 1, outfd);
				swab(&stop[x], &buf1, 2);
				fwrite(&buf1, sizeof(short), 1, outfd);
			}
		}

		/*	if (G_put_map_row (outfd, cell) < 0)
	    exit(1);
*/
	}
	if (verbose)
		percent (row, nrows, 10);

	G_close_cell (infd);
	exit(0);
}
