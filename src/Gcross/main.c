/* %W%  %G% */
#define GLOBAL
#include "glob.h"

main(argc,argv) char *argv[];
{
    int cmp();
    int fd[NFILES];
    int outfd;
    int i;
    int ok;
    char *name;
    char *mapset;
    int verbose, non_zero;
    struct Range range;
    CELL ncats, max_cats;
    int primary;
    char *options;
    struct Categories pcats;
    struct Colors pcolr;
    char buf[1024];
    CELL result;
    CELL cross();

    G_gisinit (argv[0]);

    nrows = G_window_rows();
    ncols = G_window_cols();

    nfiles = 0;
    verbose = 1;
    non_zero = 0;

/* open all cell files */
    ok = 1;
    for (i = 1; i < argc-1; i++)
    {
	if (argv[i][0] == '-')
	{
	    options = &argv[i][1];
	    if (*options == 0) options--;
	    while (*options)
	    {
		switch (*options)
		{
		case 'v': verbose = 0; break;
		case 'z': non_zero = 1; break;
		default: fprintf (stderr,"%s: %c -- unrecognized option\n",
			argv[0], *options);
			ok = 0;
		}
		options++;
	    }
	    continue;
	}
	if (nfiles >= NFILES)
	{
	    sprintf (buf, "%s: more than %d files not allowed", G_program_name(), NFILES);
	    G_fatal_error (buf);
	    exit(1);
	}
	strcpy (name = names[nfiles], argv[i]);
	mapset = G_find_cell2 (name, "");
	if (!mapset)
	{
	    sprintf (buf,"%s: [%s] not found", G_program_name(), name);
	    G_fatal_error (buf);
	    exit(1);
	}
	fd[nfiles] = G_open_cell_old (name, mapset);
	if (fd[nfiles] < 0)
	    exit(1);
	G_read_range (name, mapset, &range);
	ncats = range.nmax - range.nmin + range.pmax - range.pmin;
	if (nfiles == 0 || ncats > max_cats)
	{
	    primary = nfiles;
	    max_cats = ncats;
	}
	nfiles++;
    }
    if (nfiles <= 1)
    {
	fprintf (stderr, "usage: %s [-vz] layer1 layer2 ... result\n", G_program_name());
	exit(1);
    }
    if (!ok)
	exit(1);
    outfd = G_open_cell_new (name = argv[argc-1]);
    if (outfd < 0)
	exit(1);

    sprintf (buf, "Cross of %s", names[0]);
    for (i = 1; i < nfiles-1; i++)
    {
	strcat (buf, ", ");
	strcat (buf, names[i]);
    }
    strcat (buf, " and ");
    strcat (buf, names[i]);
    G_init_cats ((CELL) 0, buf, &pcats);

/* first step is cross product, but un-ordered */
    result = cross (fd, verbose, non_zero, primary, outfd, &pcats);

/* print message STEP mesage */
    if (verbose)
    {
	fprintf (stderr, "%s: STEP 2 ...",G_program_name());
	fflush (stderr);
    }

/* now close all files */
    for (i = 0; i < nfiles; i++)
	G_close_cell (fd[i]);
    G_close_cell (outfd);

    if (result <= 0)
	exit(0);


/* build the renumbering/reclass and the new cats file */
    qsort (reclass, result+1, sizeof(RECLASS), cmp);
    table = (CELL *) G_calloc (result+1, sizeof(CELL));

    for (ncats = 0; ncats <= result; ncats++)
    {
	table[reclass[ncats].result] = ncats;
	set_cat (ncats, reclass[ncats].cat, &pcats);
    }

    if (verbose)
	fprintf (stderr, "\n");

/* reopen the output cell for reading and for writing */
    fd[0] = G_open_cell_old (name, G_mapset());
    outfd = G_open_cell_new (name);

    renumber (fd[0], outfd, verbose);

    if (verbose)
	fprintf (stderr, "Creating support files for %s\n", name);
    G_close_cell (fd[0]);
    G_close_cell (outfd);
    G_write_cats (name, &pcats);
    G_free_cats (&pcats);
    if (result > 0)
    {
	G_make_random_colors (&pcolr, (CELL) 1, result);
	G_write_colors (name, G_mapset(), &pcolr);
    }

    printf ("%ld categories\n", (long) result);
    exit(0);
}

static
cmp (a,b) RECLASS *a, *b;
{
    int i;

    for (i = 0; i < nfiles; i++)
    {
	if (a->cat[i] < b->cat[i]) return -1;
	if (a->cat[i] > b->cat[i]) return 1;
    }
    return 0;
}
