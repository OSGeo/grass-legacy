#define GLOBAL
#include "global.h"

main(argc,argv) char *argv[];
{
    char *to_screen = " output to screen ";
    int *fd;
    int i;
    char *name;
    char *mapset;
/* flags */
    int verbose;
    int raw_data;
    int with_coordinates;
    int with_xy;
    int non_zero;
    int masking;
    int with_counts;
    int with_areas;
    int with_labels;
/* printf format */
    char fmt[20];
    int dp;
    int G_get_map_row(), G_get_map_row_nomask();
    struct
    {
	struct Flag *a ;   /* area */
	struct Flag *c ;   /* cell counts */
	struct Flag *m ;   /* report zero due to mask */
	struct Flag *l ;   /* with labels */
	struct Flag *q ;   /* quiet */
	struct Flag *z ;   /* non-zero */
	struct Flag *one ; /* one cell per line */
	struct Flag *x ;   /*    with row/col */
	struct Flag *g ;   /*    with east/north */
    } flag;
    struct
    {
	struct Option *cell ;
	struct Option *fs ;
	struct Option *output ;
    } option;

/* Define the different options */

    option.cell = G_define_option() ;
    option.cell->key        = "input";
    option.cell->type       = TYPE_STRING;
    option.cell->required   = YES;
    option.cell->multiple   = YES;
    option.cell->gisprompt  = "old,cell,raster" ;
    option.cell->description= "raster maps(s)" ;

    option.fs = G_define_option() ;
    option.fs->key        = "fs";
    option.fs->key_desc   = "character|space" ;
    option.fs->type       = TYPE_STRING;
    option.fs->required   = NO;
    option.fs->multiple   = NO;
    option.fs->answer     = "space";
    option.fs->description= "output field separator";

    option.output = G_define_option();
    option.output->key    = "output";
    option.output->type   = TYPE_STRING;
    option.output->required   = NO;
    option.output->multiple   = NO;
    option.output->description= "output file name";
    /*
    option.output->answer     = to_screen;
    */

/* Define the different flags */

    flag.one = G_define_flag() ;
    flag.one->key         = '1' ;
    flag.one->description = "One cell per line" ;

    flag.a = G_define_flag() ;
    flag.a->key         = 'a' ;
    flag.a->description = "Print area totals" ;

    flag.c = G_define_flag() ;
    flag.c->key         = 'c' ;
    flag.c->description = "Print cell counts" ;

    flag.l = G_define_flag() ;
    flag.l->key         = 'l' ;
    flag.l->description = "Print category labels" ;

    flag.m = G_define_flag() ;
    flag.m->key         = 'm' ;
    flag.m->description = "Report zero values due to mask" ;

    flag.q = G_define_flag() ;
    flag.q->key         = 'q' ;
    flag.q->description = "Quiet" ;

    flag.z = G_define_flag() ;
    flag.z->key         = 'z' ;
    flag.z->description = "Non-zero data only will be output" ;

    flag.g = G_define_flag() ;
    flag.g->key = 'g';
    flag.g->description = "Print grid coordinates (east and north) (requires -1 flag)";

    flag.x = G_define_flag() ;
    flag.x->key = 'x';
    flag.x->description = "Print x and y (column and row) (requires -1 flag)";

    G_gisinit (argv[0]);

    if (G_parser(argc, argv))
	exit (-1);

    name = option.output->answer;
    if (name != NULL && strcmp(name, to_screen) != 0)
    {
	if(NULL == freopen (name, "w", stdout))
	{
	    perror (name);
	    exit(1);
	}
    }
    nrows = G_window_rows();
    ncols = G_window_cols();

    fd = NULL;
    nfiles = 0;
    dp = -1;

    with_counts = flag.c->answer;
    with_areas = flag.a->answer;
    with_labels = flag.l->answer;

    masking = (! flag.m->answer);
    verbose = (! flag.q->answer);
    non_zero = flag.z->answer;

    raw_data = flag.one->answer;
    with_coordinates = flag.g->answer;
    with_xy = flag.x->answer;

/* turn off verbose if stdout not a tty */
    if (raw_data && verbose)
	verbose = !isatty(1);

/* get field separator */
    strcpy(fs, " ");
    if (option.fs->answer)
    {
	if (strcmp (option.fs->answer, "space") == 0)
	    *fs = ' ';
	else
	    *fs = *option.fs->answer;
    }

/* open all cell files */
    for (i = 0; name = option.cell->answers[i]; i++)
    {
	char msg[100];

	mapset = G_find_cell (name, "");
	if (!mapset)
	{
	    sprintf (msg,"%s: [%s] not found", G_program_name(), name);
	    G_fatal_error (msg);
	    exit(1);
	}
	fd = (int *) G_realloc (fd, (nfiles+1) * sizeof(int));
	fd[nfiles] = G_open_cell_old (name, mapset);
	if (fd[nfiles] < 0)
	    exit(1);
	if (with_labels) 
	{
	    labels = (struct Categories *)
		   G_realloc (labels, (nfiles+1) * sizeof(struct Categories));
	    if (G_read_cats (name, mapset, &labels[i]) < 0)
		G_init_cats((CELL) 0, "", &labels[i]);
	}
	nfiles++;
    }

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
	raw_stats (fd, verbose, non_zero, with_coordinates, with_xy, with_labels);
    else
	cell_stats (fd, verbose, non_zero, with_counts, with_areas, with_labels, fmt);
    exit(0);
}
