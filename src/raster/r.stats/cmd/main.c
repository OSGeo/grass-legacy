#define GLOBAL
#include <string.h>
#include <unistd.h>
#include "global.h"

int main (int argc, char *argv[])
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
    int with_percents;
    int with_counts;
    int with_areas;
    int with_labels;
/* printf format */
    char fmt[20];
    int dp;
    struct Range range;
    struct FPRange fp_range;
    struct Quant q;
    CELL min, max, null_set=0;
    DCELL dmin, dmax;
	struct GModule *module;
    struct
    {
	struct Flag *a ;   /* area */
	struct Flag *c ;   /* cell counts */
	struct Flag *p ;   /* percents */
	struct Flag *l ;   /* with labels */
	struct Flag *q ;   /* quiet */
	struct Flag *n ;   /* Suppress reporting of any NULLs */
	struct Flag *N ;   /* Suppress reporting of NULLs when 
			      all values are NULL */
	struct Flag *one ; /* one cell per line */
	struct Flag *x ;   /*    with row/col */
	struct Flag *g ;   /*    with east/north */
	struct Flag *i ;   /* use quant rules for fp map, i.e. read it as int */
	struct Flag *r ;   /*    raw output: when nsteps option is used,
				 report indexes of ranges instead of ranges
				 themselves; when -C (cats) option is used
				 reports indexes of fp ranges = ind. of labels */
	struct Flag *C ;   /* report stats for labeled ranges in cats files */
    } flag;
    struct
    {
	struct Option *cell ;
	struct Option *fs ;
	struct Option *nv ;
	struct Option *output ;
	struct Option *nsteps ; /* divide data range into nsteps and report stats
				   for these ranges: only for fp maps
				   NOTE: when -C flag is used, and there are 
				   explicit fp ranges in cats or when the map 
				   is int, nsteps is ignored */
    } option;

    module = G_define_module();
    module->description =
		"Generates area statistics for raster map layers.";
					        
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

    option.nv = G_define_option() ;
    option.nv->key        = "nv";
    option.nv->type       = TYPE_STRING;
    option.nv->required   = NO;
    option.nv->multiple   = NO;
    option.nv->answer     = "*";
    option.nv->description= "string representing no data cell value";

    option.output = G_define_option();
    option.output->key    = "output";
    option.output->type   = TYPE_STRING;
    option.output->required   = NO;
    option.output->multiple   = NO;
    option.output->description= "output file name";

    option.nsteps = G_define_option();
    option.nsteps->key    = "nsteps";
    option.nsteps->type   = TYPE_INTEGER;
    option.nsteps->required   = NO;
    option.nsteps->multiple   = NO;
    option.nsteps->answer     = "255";
    option.nsteps->description= "number of fp subranges to collect stats from";
    /*
    option.output->answer     = to_screen;
    */

/* Define the different flags */

    flag.one = G_define_flag() ;
    flag.one->key         = '1' ;
    flag.one->description = "One cell (range) per line" ;

    flag.a = G_define_flag() ;
    flag.a->key         = 'a' ;
    flag.a->description = "Print area totals" ;

    flag.c = G_define_flag() ;
    flag.c->key         = 'c' ;
    flag.c->description = "Print cell counts" ;

    flag.p = G_define_flag() ;
    flag.p->key         = 'p' ;
    flag.p->description = "Print APPROXIMATE percents (total percent may not be 100%)" ;

    flag.l = G_define_flag() ;
    flag.l->key         = 'l' ;
    flag.l->description = "Print category labels" ;

    flag.q = G_define_flag() ;
    flag.q->key         = 'q' ;
    flag.q->description = "Quiet" ;

    flag.n = G_define_flag() ;
    flag.n->key         = 'n' ;
    flag.n->description = "Suppress reporting of any NULLs" ;

    flag.N = G_define_flag() ;
    flag.N->key         = 'N' ;
    flag.N->description = "Suppress reporting of NULLs when all values are NULL" ;
    flag.g = G_define_flag() ;
    flag.g->key = 'g';
    flag.g->description = "Print grid coordinates (east and north) (requires -1 flag)";

    flag.x = G_define_flag() ;
    flag.x->key = 'x';
    flag.x->description = "Print x and y (column and row) (requires -1 flag)";

    flag.C = G_define_flag() ;
    flag.C->key         = 'C' ;
    flag.C->description = "report for cats fp ranges (fp maps only)" ;

    flag.r = G_define_flag() ;
    flag.r->key = 'r';
    flag.r->description = "Print raw indexes of fp ranges (fp maps only)";

    flag.i = G_define_flag() ;
    flag.i->key = 'i';
    flag.i->description = "Read fp map as integer (use map's quant rules)";

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
    sscanf(option.nsteps->answer, "%d", &nsteps);
    if(nsteps <= 0)
    {
         G_warning("%s: nsteps has to be > 0; using nsteps=255");
	 nsteps = 255;
    }
    cat_ranges = flag.C->answer;

    raw = flag.r->answer;
    as_int = flag.i->answer;
    nrows = G_window_rows();
    ncols = G_window_cols();


    fd = NULL;
    nfiles = 0;
    dp = -1;

    with_percents = flag.p->answer;
    with_counts = flag.c->answer;
    with_areas = flag.a->answer;
    with_labels = flag.l->answer;

    verbose = (! flag.q->answer);
    no_nulls = flag.n->answer;
    no_nulls_all = flag.N->answer;
    no_data_str = option.nv->answer;

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
	}
	fd = (int *) G_realloc (fd, (nfiles+1) * sizeof(int));
        is_fp = (int *) G_realloc (is_fp, (nfiles+1) * sizeof(int));
        DMAX = (DCELL *) G_realloc (DMAX, (nfiles+1) * sizeof(DCELL));
        DMIN = (DCELL *) G_realloc (DMIN, (nfiles+1) * sizeof(DCELL));
        
	fd[nfiles] = G_open_cell_old (name, mapset);
	if (fd[nfiles] < 0)
	    exit(1);
	if(!as_int)
            is_fp[nfiles] = G_raster_map_is_fp(name, mapset);
        else 
	{
	    is_fp[nfiles] = 0;
	    if(cat_ranges || nsteps != 255)
	    {
	         sprintf(msg, "%s: -i means read %s as integer! -C flag and/or nsteps option will be ignored", G_program_name(),name);
	         G_warning(msg);
            }
        }
	if (with_labels || (cat_ranges && is_fp[nfiles])) 
	{
	    labels = (struct Categories *)
		   G_realloc (labels, (nfiles+1) * sizeof(struct Categories));
	    if (G_read_cats (name, mapset, &labels[i]) < 0)
		G_init_cats((CELL) 0, "", &labels[i]);
	}
	if(is_fp[nfiles])
	/* floating point map */
	{
	   G_quant_init(&q);
	   if(cat_ranges)
	   {
	      if(! G_quant_nof_rules (&labels[i].q))
	      {
	         sprintf(msg, "%s: cats for %s sre either missing or have no explicit labels. Using nsteps=%d", G_program_name(),name, nsteps);
	         G_warning(msg);
		 cat_ranges = 0;
              }
	      else if (nsteps != 255)
	      {
	         sprintf(msg, "%s: -C flag was given, using cats fp ranges of %s, ignoring nsteps option", G_program_name(),name);
	         G_warning(msg);
              }
           }
	   if(!cat_ranges) /* DO NOT use else here, cat_ranges can change */
	   {
	      if(G_read_fp_range (name, mapset, &fp_range) < 0)
	      {
	         sprintf (msg,"%s: can't read fp range for [%s]",G_program_name(),name);
	         G_fatal_error (msg);
              }
	      G_get_fp_range_min_max (&fp_range, &DMIN[nfiles], &DMAX[nfiles]);
 	      G_quant_add_rule (&q, DMIN[nfiles], DMAX[nfiles], 1, nsteps);
	      /* set the quant rules for reading the map */
	      G_set_quant_rules(fd[nfiles], &q); 
	      G_quant_get_limits (&q, &dmin, &dmax, &min, &max);
	      G_quant_free (&q);
           }
	   else /* cats ranges */
	   {
	      /* set the quant rules for reading the map */
	      G_set_quant_rules(fd[nfiles], &labels[i].q); 
	      G_quant_get_limits (&labels[i].q, &dmin, &dmax, &min, &max);
           }
        }
	else
	{
	   if(G_read_range (name, mapset, &range) < 0)
	   {
	      sprintf (msg,"%s: can't read range for [%s]",G_program_name(),name);
	      G_fatal_error (msg);
           }
	   G_get_range_min_max (&range, &min, &max);
        }
	if(!null_set)
	{
	   null_set = 1;
	   NULL_CELL = max + 1;
        }
	else if(NULL_CELL < max+1) NULL_CELL = max + 1;
	nfiles++;
    }

    if (dp < 0)
	strcpy (fmt, "%lf");
    else
	sprintf (fmt, "%%.%dlf", dp);

    if (raw_data)
	raw_stats (fd, verbose, with_coordinates, with_xy, with_labels);
    else
	cell_stats (fd, verbose, with_percents, with_counts, with_areas, with_labels, fmt);
    exit(0);
}
