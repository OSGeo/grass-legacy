/* %W% %G% */

#include "nfiles.h"
#include "gis.h"

main (argc, argv) char *argv[];
{
    int infd[MAXFILES];
    struct Cell_stats statf[MAXFILES];
    struct Categories cats;
    struct Colors colr;
    int cats_ok;
    int colr_ok;
    int outfd;
    CELL *result, *patch;
    int nfiles;
    int i;
    int ok;
    int row,nrows,ncols;
    int verbose;
    char *me, *name, *mapset;
    char *new_name;

    me = argv[0];
    argc--;
    argv++;

    verbose = 1;
    if (argc > 0 && argv[0][0] == '-')
    {
	if (strcmp (argv[0], "-v") == 0)
	    verbose = 0;
	else
	{
	    fprintf (stderr, "%s - illegal option\n", argv[0]);
	    usage (me);
	}
	argc--;
	argv++;
    }
    if (argc < 3)
	usage(me);

    nfiles = argc-1;
    if (nfiles >= MAXFILES)
    {
	fprintf (stderr, "%s - too many patch files. only %d allowed\n",
	    me, MAXFILES);
	exit(1);
    }

    G_gisinit (me);

    ok = 1;
    for (i = 0; i < nfiles; i++)
    {
	name = argv[i];
	mapset = G_find_cell (name, "");
	if (mapset == NULL)
	{
	    fprintf (stderr, "%s - %s not found\n", me, name);
	    sleep(3);
	    ok = 0;
	}
	if (!ok) continue;
	infd[i] = G_open_cell_old (name, mapset);
	if (infd[i] < 0)
	{
	    ok = 0;
	    continue;
	}
	G_init_cell_stats (&statf[i]);
    }
    if (!ok)
	exit(1);

    outfd = G_open_cell_new (new_name = argv[argc-1]);
    if (outfd < 0)
	exit(1);
    
    result = G_allocate_cell_buf();
    patch  = G_allocate_cell_buf();

    nrows = G_window_rows();
    ncols = G_window_cols();

    if (verbose) printf ("percent complete: ");
    for (row = 0; row < nrows; row++)
    {
	if (verbose) G_percent (row, nrows, 10);
	if(G_get_map_row (infd[0], result, row) < 0)
	    exit(1);
	G_update_cell_stats (result, ncols, &statf[0]);
	for (i = 1; i < nfiles; i++)
	{
	    if(G_get_map_row (infd[i], patch, row) < 0)
		exit(1);
	    if(!do_patch (result, patch, &statf[i], ncols))	
		break;
	}
	G_put_map_row (outfd, result);
    }
    if (verbose) G_percent (row, nrows, 10);

    free (patch);
    free (result);
    for (i = 0; i < nfiles; i++)
	G_close_cell (infd[i]);
/* 
 * build the new cats and colors. do this before closing the new
 * file, in case the new file is one of the patching files as well.
 */
    if (verbose) printf ("CREATING SUPPORT FILES FOR %s\n", new_name);
    support (argv, statf, nfiles, &cats, &cats_ok, &colr, &colr_ok);

/* now close (and create) the result */
    G_close_cell (outfd);
    if (cats_ok)
	G_write_cats (new_name, &cats);
    if (colr_ok)
	G_write_colors (new_name, G_mapset(), &colr);
    exit(0);
}

usage (me) char *me;
{
    fprintf (stderr,"usage: %s [-v] patch1 patch2 [patch3 ... patch%d] result\n",
	me, MAXFILES);
    exit(1);
}
