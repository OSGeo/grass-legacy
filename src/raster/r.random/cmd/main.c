#include "gis.h"

main (argc, argv)
    int  argc;
    char *argv[];
{
    int i;
    short percent;
    double percentage;
    long targets;
    long count, get_count();
    int zero, verbose;
    char *mapset;
    CELL cat_zero;
    char msg[100];

    struct
    {
	struct Option *input, *raster, *sites, *npoints;
    } parm;
    struct
    {
	struct Flag *zero, *quiet ;
    } flag;

    parm.input = G_define_option() ;
    parm.input->key        = "input" ;
    parm.input->type       = TYPE_STRING ;
    parm.input->required   = YES ;
    parm.input->gisprompt  = "old,cell,raster" ;
    parm.input->description= "Name of existing raster map" ;

    parm.npoints = G_define_option() ;
    parm.npoints->key        = "nsites" ;
    parm.npoints->key_desc   = "number[%]";
    parm.npoints->type       = TYPE_STRING ;
    parm.npoints->required   = YES ;
    parm.npoints->description= "The number of sites to allocate";

    parm.raster = G_define_option() ;
    parm.raster->key        = "raster_output" ;
    parm.raster->type       = TYPE_STRING ;
    parm.raster->required   = NO;
    parm.raster->gisprompt  = "new,cell,raster" ;
    parm.raster->description= "Name of the output raster map" ;

    parm.sites = G_define_option() ;
    parm.sites->key        = "sites_output" ;
    parm.sites->type       = TYPE_STRING ;
    parm.sites->required   = NO ;
    parm.sites->gisprompt  = "new,site_lists,sites" ;
    parm.sites->description= "Name of the output sites map";

    flag.quiet = G_define_flag() ;
    flag.quiet->key         = 'q' ;
    flag.quiet->description = "Run quietly" ;

    flag.zero = G_define_flag() ;
    flag.zero->key         = 'z' ;
    flag.zero->description = "Generate sites for category zero also";

    G_gisinit (argv[0]);

/* if first arg is "####n" scarf it up
 * since it was put there by inter/r.random
 */
    count = 0;
    if (argc > 1)
    {
	long x;
	if (sscanf (argv[1], "####%ld", &x) == 1 && x > 0)
	{
	    argc--;
	    for (i = 1; i < argc; i++)
		argv[i] = argv[i+1];
	    count = x;
	}
    }

    if (G_parser(argc, argv) || !(parm.raster->answer || parm.sites->answer))
    {
	fprintf (stderr, "\nNote: one (or both) of %s and %s must be specified\n",
		parm.raster->key, parm.sites->key);
	exit(-1);
    }
    verbose = ! flag.quiet->answer;
    zero    =   flag.zero->answer;

    mapset = G_find_cell2 (parm.input->answer, "");
    if (mapset == NULL)
    {
	fprintf (stderr, "%s: <%s> raster file not found",
	    G_program_name(), parm.input->answer);
	exit(1);
    }
    if (parm.raster->answer && G_legal_filename (parm.raster->answer) < 0)
    {
	fprintf (stderr, "%s: <%s> illegal file name",
	    G_program_name(), parm.raster->answer);
	exit(1);
    }
    if (parm.sites->answer && G_legal_filename (parm.sites->answer) < 0)
    {
	fprintf (stderr, "%s: <%s> illegal file name",
	    G_program_name(), parm.sites->answer);
	exit(1);
    }

    /* look for n[%] */
    percent = has_percent(parm.npoints->answer);
    if (percent)
    {
	if (sscanf (parm.npoints->answer, "%lf", &percentage) != 1
	|| percentage <= 0.0 || percentage > 100.0)
	{
	    fprintf (stderr, "<%s=%s> invalid percentage\n",
		parm.npoints->key, parm.npoints->answer);
	    G_usage();
	    exit(1);
	}
    }
    else
    {
	if (sscanf (parm.npoints->answer, "%ld", &targets) != 1
	|| targets <= 0)
	{
	    fprintf (stderr, "<%s=%s> invalid number of points\n",
		parm.npoints->key, parm.npoints->answer);
	    G_usage();
	    exit(1);
	}
    }
    cat_zero = 0;
    if (count <= 0 || zero)
	count = get_count (parm.input->answer, mapset, &cat_zero, zero, verbose);

    if (verbose)
	fprintf (stderr, "%ld data cells\n", count);

    if (percent)
    {
	long x;
	x = (long) (count * percentage / 100.0 +.5);
	targets = x;
	if (x != targets)
	    G_fatal_error ("Too many random locations for this algorithm");
    }
    else if (targets > count)
    {
	sprintf (msg,
	    "%s: There aren't %ld %scells in the current region",
		G_program_name(), targets, zero?"":"non-zero ");
	G_fatal_error (msg);
	exit(1);
    }
    if (targets <= 0)
    {
	sprintf (msg,
	    "%s: There aren't any valid locations in the current region",
	    G_program_name());
	G_fatal_error (msg);
	exit(1);
    }

    execute_random (targets, count, parm.input->answer, mapset, cat_zero,
		parm.raster->answer, parm.sites->answer, verbose);

    if (parm.raster->answer)
	make_support (parm.input->answer, mapset, parm.raster->answer,
		      parm.npoints->answer, percent, cat_zero);


    exit(0);
}

static
has_percent(s)
    char *s;
{
    while (*s)
	if (*s++ == '%')
	    return 1;
    return 0;
}
