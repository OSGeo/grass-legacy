#define GLOBAL
#include "glob.h"
#include <stdio.h>

main(argc,argv) 
    int   argc;
    char *argv[];
{
    int cmp();
    int fd[NFILES];
    int outfd;
    int i;
    char *name;
    char *output;
    char *mapset;
    int verbose, non_zero;
    struct Range range;
    CELL ncats, max_cats;
    int primary;
    struct Categories pcats;
    struct Colors pcolr;
    char buf[1024];
    CELL result;
    CELL cross();
    struct
    {
	struct Option *input, *output;
    } parm;
    struct
    {
	struct Flag *q, *z;
    } flag;

    G_gisinit (argv[0]);

/* Define the different options */

    parm.input = G_define_option() ;
    parm.input->key        = "input";
    parm.input->type       = TYPE_STRING;
    parm.input->required   = YES;
    parm.input->multiple   = YES;	
    parm.input->gisprompt  = "old,cell,raster" ;
    sprintf(parm.input->description= G_malloc(60),
	"Names of 2-%d input raster maps", NFILES);

    parm.output = G_define_option() ;
    parm.output->key        = "output";
    parm.output->type       = TYPE_STRING;
    parm.output->required   = YES;
    parm.output->description= "Name of the resulting map";
    parm.output->gisprompt  = "new,cell,raster" ;

/* Define the different flags */

    flag.q = G_define_flag() ;
    flag.q->key         = 'q' ;
    flag.q->description = "Quiet" ;

    flag.z = G_define_flag() ;
    flag.z->key         = 'z' ;
    flag.z->description = "Non-zero data only" ;

    if (G_parser(argc, argv))
	exit (1);

    nrows = G_window_rows();
    ncols = G_window_cols();

    nfiles = 0;
    verbose = 1;
    non_zero = 0;
  

    verbose  = (! flag.q->answer);
    non_zero = flag.z->answer;

    for (nfiles = 0; name = parm.input->answers[nfiles]; nfiles++)
    {
        if (nfiles >= NFILES)
        {
            sprintf (buf, "%s: more than %d files not allowed", G_program_name(), NFILES);
            G_fatal_error (buf);
            exit(1);
        }
        mapset = G_find_cell2 (name, "");
        if (!mapset)
        {
            sprintf (buf,"%s: [%s] not found", G_program_name(), name);
            G_fatal_error (buf);
            exit(1);
        }
        names[nfiles] = name;
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
    }
   
    if (nfiles <= 1)
    {
	fprintf(stderr, "ERROR ** must specify 2 or more input maps **\n");
	G_usage();
	exit (-1);
    } 
    output = parm.output->answer; 
    outfd = G_open_cell_new (output);

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
    result = cross (fd, verbose, non_zero, primary, outfd);

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
    for (i =0; i < nfiles; i++)
    {
	mapset = G_find_cell (names[i], "");
	G_read_cats (names[i], mapset, &labels[i]);
    }

    for (ncats = 0; ncats <= result; ncats++)
    {
	table[reclass[ncats].result] = ncats;
	set_cat (ncats, reclass[ncats].cat, &pcats);
    }

    for (i = 0; i < nfiles; i++)
	G_free_cats (&labels[i]);

    if (verbose)
	fprintf (stderr, "\n");

/* reopen the output cell for reading and for writing */
    fd[0] = G_open_cell_old (output, G_mapset());
    outfd = G_open_cell_new (output);

    renumber (fd[0], outfd, verbose);

    if (verbose)
	fprintf (stderr, "Creating support files for %s\n", output);
    G_close_cell (fd[0]);
    G_close_cell (outfd);
    G_write_cats (output, &pcats);
    G_free_cats (&pcats);
    if (result > 0)
    {
	G_make_random_colors (&pcolr, (CELL) 1, result);
	G_write_colors (output, G_mapset(), &pcolr);
    }

    printf ("%ld categories\n", (long) result);
    exit(0);
}

static
cmp (a,b) RECLASS *a, *b;
{
    int i;

    for (i = 0; i < (nfiles + 2); i++)
    {
	if (a->cat[i] < b->cat[i]) return -1;
	if (a->cat[i] > b->cat[i]) return 1;
    }
    return 0;
}
