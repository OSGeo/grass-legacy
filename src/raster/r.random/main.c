#include "gis.h"
#include "local_proto.h"

static int has_percent(char *);

void
do_info_exit(struct rr_state *);

int 
main (int argc, char *argv[])
{
    int i;
    short percent;
    double percentage;
    long targets;
    long count;
    int zero, verbose;
    char *mapset;
    CELL cat_zero;
    char msg[100];
    struct rr_state  myState;

    struct
    {
	struct Option *input, *raster, *sites, *npoints;
    } parm;
    struct
    {
	struct Flag *zero, *quiet, *info ;
    } flag;

    G_gisinit (argv[0]);

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

    flag.info = G_define_flag() ;
    flag.info->key         = 'i' ;
    flag.info->description = "Report information about input raster and exit";
    

    if (G_parser(argc, argv) != 0)
        exit(0);
    
    /* Set some state variables */
    myState.verbose   = (flag.quiet->answer) ? 0 : 1;
    myState.use_nulls = flag.zero->answer;
    myState.inraster  = parm.input->answer;
    myState.outraster = parm.raster->answer;
    myState.outsites  = parm.sites->answer;
    
    myState.mapset = G_find_cell (myState.inraster, "");
    if (myState.mapset == NULL)
    {
	G_fatal_error ("%s: <%s> raster file not found",
	    G_program_name(), myState.inraster);
    }

    /* If they only want info we ignore the rest */
    get_stats(&myState);
    if (flag.info->answer)
        do_info_exit(&myState);

    if (!(parm.raster->answer || parm.sites->answer))
    {
	G_fatal_error ("\nNote: one (or both) of %s and %s must be specified\n",
		parm.raster->key, parm.sites->key);
    }
    
    if (myState.outraster)
    {
        if (G_legal_filename (myState.outraster) < 0)
        {
            fprintf (stderr, "%s: <%s> illegal file name",
                G_program_name(), myState.outraster);
            exit(1);
        }
        if (G_find_cell2 (myState.outraster, G_mapset()) != NULL)
        {
            G_fatal_error("%s: Output raster <%s> exists!",
                    G_program_name(), myState.outraster);
        }
    }

    if (myState.outsites)
    {
        if (G_legal_filename (myState.outsites) < 0)
        {
            G_fatal_error( "%s: <%s> illegal file name",
                G_program_name(), myState.outsites);
        }
        if (G_find_file ("sites", myState.outsites, G_mapset()) != NULL)
        {
            G_fatal_error("%s: Sites files <%s> exists!",
                    G_program_name(), myState.outsites);
        }
    }

    /* look for n[%] */
    percent = has_percent(parm.npoints->answer);
    if (percent)
    {
	if (sscanf (parm.npoints->answer, "%lf", &percentage) != 1
	|| percentage <= 0.0 || percentage > 100.0)
	{
	    G_fatal_error ( "<%s=%s> invalid percentage\n",
		parm.npoints->key, parm.npoints->answer);
	}
    }
    else
    {
	if (sscanf (parm.npoints->answer, "%ld", &targets) != 1
	|| targets <= 0)
	{
	    G_fatal_error ( "<%s=%s> invalid number of points\n",
		parm.npoints->key, parm.npoints->answer);
	}
    }

    count = (myState.use_nulls) ? myState.nCells : 
        myState.nCells - myState.nNulls;
    
    if (percent)
    {
	myState.nRand = (int) (count * percentage / 100.0 +.5);
    }
    else 
    {
        if (targets > count)
        {
            sprintf (msg,
                "%s: There aren't %ld %scells in the current region",
                    G_program_name(), targets, zero?"":"non-zero ");
            G_fatal_error (msg);
        }
        if (targets <= 0)
        {
            sprintf (msg,
                "%s: There aren't any valid locations in the current region",
                G_program_name());
            G_fatal_error (msg);
            exit(1);
        }
        myState.nRand = targets;
    }

    execute_random (&myState);
    
    if (myState.outraster)
	make_support (&myState, percent);

    return 0;
}

static int has_percent(char *s)
{
    while (*s)
	if (*s++ == '%')
	    return 1;
    return 0;
}

void
do_info_exit(struct rr_state *myState)
{
    fprintf (stdout,
            "Raster:      %s@%s\n"
            "Cell Count:  %d\n"
            "Null Cells:  %d\n\n",
            myState->inraster, myState->mapset,
            myState->nCells, myState->nNulls);
    exit(0);
}


/* vim: set softtabstop=4 shiftwidth=4 expandtab: */
