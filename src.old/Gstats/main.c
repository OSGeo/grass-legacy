#define GLOBAL
#include "global.h"

main(argc,argv) char *argv[];
{
    int fd[NFILES];
    int i;
    int ok;
    char *name;
    char *mapset;
    int verbose;
    int raw_data;
    int non_zero;
    int masking;
    char fmt[20];
    int dp;
    int cell_counts;
    struct Range range;
    CELL ncats, max_cats;
    char *options;
    int G_get_map_row(), G_get_map_row_nomask();

    G_gisinit ("Gstats");

    nrows = G_window_rows();
    ncols = G_window_cols();

    nfiles = 0;
    dp = -1;
    verbose = 1;
    raw_data = 0;
    non_zero = 0;
    cell_counts = 0;
    masking = 1;

/* open all cell files */
    ok = 1;
    for (i = 1; i < argc; i++)
    {
	char msg[100];

	if (argv[i][0] == '-')
	{
	    options = &argv[i][1];
	    if (*options == 0) options--;
	    while (*options)
	    {
		switch (*options)
		{
		case 'c': cell_counts = 1; break;
		case 'C': cell_counts = 2; break;
		case 'n': masking = 0; break;
		case 'v': verbose = 0; break;
		case 'r': raw_data = 1; break;
		case 'R': raw_data = 2; break;
		case 'z': non_zero = 1; break;
		default: fprintf (stderr,"%s: %c -- unrecognized option\n",
			argv[0], *options);
			ok = 0;
		}
		options++;
	    }
	    continue;
	}
	if (raw_data && verbose)
	    verbose = !isatty(1);

	if (nfiles >= NFILES)
	{
	    sprintf (msg, "%s: more than %d files not allowed", G_program_name(), NFILES);
	    G_fatal_error (msg);
	    exit(1);
	}
	mapset = G_find_file ("cell", name = argv[i], "");
	if (!mapset)
	{
	    sprintf (msg,"%s: [%s] not found", G_program_name(), name);
	    G_fatal_error (msg);
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
    if (nfiles <= 0)
    {
	fprintf (stderr, "Usage: %s [-cnrvz] layer1 [layer2] ...\n", G_program_name());
	exit(1);
    }
    if (!ok) exit(1);

    if (dp < 0)
	strcpy (fmt, "%lf");
    else
	sprintf (fmt, "%%.%dlf", dp);

/* If -n specified, do not process MASK specially
 * Otherwise ignore 0 values that are caused by the MASK
 */
    mask = G_allocate_cell_buf();
    maskfd = masking ? G_maskfd() : -1 ;
    if (maskfd >= 0)
    {
	get_row = G_get_map_row_nomask;
    }
    else
    {
	get_row = G_get_map_row;
	for (i = 0; i < ncols; i++)
	    mask[i] = 1;
    }

    if (raw_data)
	raw_stats (fd, verbose, non_zero, raw_data>1);
    else
	cell_stats (fd, verbose, non_zero, cell_counts, fmt);
    exit(0);
}
