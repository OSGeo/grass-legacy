#include "gis.h"
main(argc, argv) char *argv[];
{
    int nrows, ncols;
    CELL **cell;
    char *mapset;
    double *sum, **sum2;
    double count;
    int *fd;
    int nfiles;
    int i,j;
    int row, col;

    G_gisinit (argv[0]);

    argc--;
    argv++;
    nfiles = argc;
    if (!nfiles) usage();

    fd   = (int *) G_malloc (nfiles * sizeof(int));
    cell = (CELL **) G_malloc (nfiles * sizeof (CELL *));
    sum  = (double *) G_malloc (nfiles * sizeof(double));
    sum2 = (double **) G_malloc (nfiles * sizeof(double *));
    for (i = 0; i < nfiles; i++)
    {
	sum2[i] = (double *) G_malloc (nfiles * sizeof(double));
	cell[i] = G_allocate_cell_buf();
	mapset = G_find_cell(argv[i], "");
	if (!mapset)
	{
	    fprintf(stderr, "%s - cell file not found\n", argv[i]);
	    exit(1);
	}
	fd[i] = G_open_cell_old (argv[i], mapset);
	if (fd[i] < 0)
	{
	    fprintf(stderr, "%s - can't open cell file\n", argv[i]);
	    exit(1);
	}
    }

    nrows = G_window_rows();
    ncols = G_window_cols();
    count = nrows * ncols;
    if (count <= 1.1)
    {
	fprintf (stderr, "Window too small\n");
	exit(1);
    }

    fprintf (stderr, "%s: complete ... ", G_program_name());
    for (row = 0; row < nrows; row++)
    {
	G_percent (row, nrows, 2);
	for (i = 0; i < nfiles; i++)
	{
	    if (G_get_map_row (fd[i], cell[i], row) < 0)
		exit(1);
	}
	for (col = 0; col < ncols; col++)
	{
	    for (i = 0; i < nfiles; i++)
	    {
		sum[i] += (double) cell[i][col];
		for (j = 0; j <= i; j++)
		    sum2[j][i] += (double) cell[i][col]*cell[j][col];
	    }
	}
    }
    G_percent (row, nrows, 2);

    for (i = 0; i < nfiles; i++)
    {
	for (j = 0; j <= i; j++)
	    printf ("%lf ", (sum2[j][i] - sum[i]*sum[j]/count)/(count-1));
	for (j = i+1; j < nfiles; j++)
	    printf ("%lf ", (sum2[i][j] - sum[i]*sum[j]/count)/(count-1));
	printf ("\n");
    }
    exit(0);
}
usage()
{
    fprintf (stderr, "Usage: %s cellfile(s)\n", G_program_name());
    exit(0);
}
