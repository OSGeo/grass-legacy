#include "nfiles.h"
#include "gis.h"

main (argc, argv) 
int   argc;
char *argv[];
{
    int infd[MAXFILES];
    struct Cell_stats statf[MAXFILES];
    struct Categories cats;
    struct Colors colr;
    int cats_ok;
    int colr_ok;
    int outfd;
    CELL *presult, *patch;
    int nfiles;
    char *rname; 
    int i;
    int ok;
    int row,nrows,ncols;
    int verbose;
    char *name, *mapset;
    char *new_name;
    char **names;
    char **ptr; 
    struct Flag *flag1 ;
    struct Option *opt1, *opt2 ;

/* Define the different options */

    opt1 = G_define_option() ;
    opt1->key        = "input";
    opt1->type       = TYPE_STRING;
    opt1->required   = YES;
    opt1->multiple   = YES;
    opt1->gisprompt  = "old,cell,raster" ;
    opt1->description= "Name of raster maps to be patched together" ;

    opt2 = G_define_option() ;
    opt2->key        = "output";
    opt2->type       = TYPE_STRING;
    opt2->required   = YES;
    opt2->gisprompt  = "new,cell,raster" ;
    opt2->description= "Name of the result map";

/* Define the different flags */

    flag1 = G_define_flag() ;
    flag1->key         = 'q' ;
    flag1->description = "Quiet" ;

    verbose = 1;
    nfiles = 0;

    G_gisinit (argv[0]);

    if (G_parser(argc, argv))
        exit(-1);

    verbose = (!flag1->answer);

    ok = 1;
    names = opt1->answers;
    ptr = opt1->answers;
    for (; *ptr != NULL; ptr++)
    {
        if (nfiles >= MAXFILES)
        {
            fprintf (stderr, "%s - too many patch files. only %d allowed\n",
            G_program_name(), MAXFILES);
            exit(1);
        }

        name = *ptr;
        mapset = G_find_cell2 (name, "");
        if (mapset == NULL)
        {
            fprintf (stderr, "%s - %s not found\n", G_program_name(), name);
            sleep(3);
            ok = 0;
        }
        if (!ok) 
            continue;
        infd[nfiles] = G_open_cell_old (name, mapset);
        if (infd[nfiles] < 0)
        {
            ok = 0;
            continue;
        }
        G_init_cell_stats (&statf[nfiles]);
    
        nfiles++;
    }

    if (!ok)
        exit(1);

    if (nfiles <= 1)
    {
        fprintf(stderr, "Error:The min specified input map is two\n");
        exit (-1);
    }

    rname = opt2->answer;
    outfd = G_open_cell_new (new_name = rname);
    if (outfd < 0)
	exit(1);
    
    presult = G_allocate_cell_buf();
    patch  = G_allocate_cell_buf();

    nrows = G_window_rows();
    ncols = G_window_cols();

    if (verbose) fprintf (stderr, "%s: percent complete: ", G_program_name());
    for (row = 0; row < nrows; row++)
    {
	if (verbose) G_percent (row, nrows, 2);
	if(G_get_map_row (infd[0], presult, row) < 0)
	    exit(1);
	G_update_cell_stats (presult, ncols, &statf[0]);
	for (i = 1; i < nfiles; i++)
	{
	    if(G_get_map_row (infd[i], patch, row) < 0)
		exit(1);
	    if(!do_patch (presult, patch, &statf[i], ncols))	
		break;
	}
	G_put_map_row (outfd, presult);
    }
    if (verbose) G_percent (row, nrows, 2);

    free (patch);
    free (presult);
    for (i = 0; i < nfiles; i++)
	G_close_cell (infd[i]);
/* 
 * build the new cats and colors. do this before closing the new
 * file, in case the new file is one of the patching files as well.
 */
    if (verbose) 
        printf ("CREATING SUPPORT FILES FOR %s\n", new_name);
    support (names, statf, nfiles, &cats, &cats_ok, &colr, &colr_ok);

/* now close (and create) the result */
    G_close_cell (outfd);
    if (cats_ok)
	G_write_cats (new_name, &cats);
    if (colr_ok)
	G_write_colors (new_name, G_mapset(), &colr);
    exit(0);
}

