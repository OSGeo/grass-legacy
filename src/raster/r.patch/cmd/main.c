#include <unistd.h>
#include "gis.h"
#include "nfiles.h"
#include "local_proto.h"

int main (int argc, char *argv[])
{
    int infd[MAXFILES];
    struct Categories cats;
    struct Cell_stats statf[MAXFILES];
    struct Colors colr;
    int cats_ok;
    int colr_ok;
    int outfd;
    RASTER_MAP_TYPE out_type, map_type;
    void *presult, *patch;
    int nfiles;
    char *rname; 
    int i;
    int ok;
    int row,nrows,ncols;
    int verbose;
    int ZEROFLAG;
    char *name, *mapset;
    char *new_name;
    char **names;
    char **ptr; 
	struct GModule *module;
    struct Flag *flag1 ;
    struct Flag *zeroflag;
    struct Option *opt1, *opt2 ;

    module = G_define_module();
	module->description =
		"Creates a composite raster map layer by using "
		"known category values from one (or more) map layer(s) "
		"to fill in areas of \"no data\" in another map layer.";

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

    zeroflag = G_define_flag() ;
    zeroflag->key         = 'z' ;
    zeroflag->description = "Use zero (0) for transparency instead of NULL" ;

    verbose = 1;
    ZEROFLAG = 0; /* default: use NULL for transparency */
    nfiles = 0;

    G_gisinit (argv[0]);

    if (G_parser(argc, argv))
        exit(-1);

    verbose = (!flag1->answer);
    ZEROFLAG= (zeroflag->answer);
    
    ok = 1;
    names = opt1->answers;
    ptr = opt1->answers;
    out_type = CELL_TYPE;
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

        map_type = G_raster_map_type(name, mapset);
	if(map_type==FCELL_TYPE && out_type == CELL_TYPE)
	       out_type = FCELL_TYPE;
        else if(map_type==DCELL_TYPE) 
	       out_type = DCELL_TYPE;

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
    outfd = G_open_raster_new (new_name = rname, out_type);
    if (outfd < 0)
	exit(1);
    
    presult = G_allocate_raster_buf(out_type);
    patch  = G_allocate_raster_buf(out_type);

    nrows = G_window_rows();
    ncols = G_window_cols();

    if (verbose) fprintf (stderr, "%s: percent complete: ", G_program_name());
    for (row = 0; row < nrows; row++)
    {
	if (verbose) G_percent (row, nrows, 2);
	if(G_get_raster_row (infd[0], presult, row, out_type) < 0)
	    exit(1);
        if(out_type == CELL_TYPE)
            G_update_cell_stats ((CELL *) presult, ncols, &statf[0]);
	for (i = 1; i < nfiles; i++)
	{
	    if(G_get_raster_row (infd[i], patch, row, out_type) < 0)
		exit(1);
	    if(!do_patch (presult, patch, &statf[i], ncols, out_type, ZEROFLAG))
		break;
	}
	G_put_raster_row (outfd, presult, out_type);
    }
    if (verbose) G_percent (row, nrows, 2);

    G_free (patch);
    G_free (presult);
    for (i = 0; i < nfiles; i++)
	G_close_cell (infd[i]);
/* 
 * build the new cats and colors. do this before closing the new
 * file, in case the new file is one of the patching files as well.
 */
    if (verbose) 
        fprintf (stdout,"CREATING SUPPORT FILES FOR %s\n", new_name);
    support (names, statf, nfiles, &cats, &cats_ok, &colr, &colr_ok, out_type);

/* now close (and create) the result */
    G_close_cell (outfd);
    if (cats_ok)
	G_write_cats (new_name, &cats);
    if (colr_ok)
	G_write_colors (new_name, G_mapset(), &colr);
    exit(0);
}


