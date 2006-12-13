#include <stdlib.h>
#include <grass/gis.h>
#include "local_proto.h"
#include <grass/glocale.h>

static int has_percent(char *);

void
do_info_exit(struct rr_state *);

int 
main (int argc, char *argv[])
{
    short percent;
    double percentage;
    long targets;
    long count;
    int zero;
    char msg[100];
    struct rr_state  myState;

    struct GModule *module;
    struct
    {
	struct Option *input, *raster, *sites, *npoints;
    } parm;
    struct
    {
	struct Flag *zero, *info, *z_geometry ;

        /* please, remove before GRASS 7 released */
        struct Flag *quiet;
    } flag;

    G_gisinit (argv[0]);

    module = G_define_module();
    module->keywords = _("raster");
    module->description =
        _("Creates a raster map layer and vector point map "
        "containing randomly located sites.");

    parm.input = G_define_standard_option(G_OPT_R_INPUT) ;
    parm.input->description= _("Name of existing raster map") ;

    parm.npoints = G_define_option() ;
    parm.npoints->key        = "n" ;
    parm.npoints->key_desc   = "number[%]";
    parm.npoints->type       = TYPE_STRING ;
    parm.npoints->required   = YES ;
    parm.npoints->description= _("The number of points to allocate");

    parm.raster = G_define_standard_option(G_OPT_R_OUTPUT) ;
    parm.raster->required   = NO ;
    parm.raster->key        = "raster_output" ;

    parm.sites = G_define_standard_option(G_OPT_V_OUTPUT) ;
    parm.sites->required   = NO ;
    parm.sites->key        = "vector_output" ;

    /* please, remove before GRASS 7 released */
    flag.quiet = G_define_flag() ;
    flag.quiet->key         = 'q' ;
    flag.quiet->description = _("Run quietly") ;

    flag.zero = G_define_flag() ;
    flag.zero->key         = 'z' ;
    flag.zero->description = _("Generate vector points for category zero also");

    flag.info = G_define_flag() ;
    flag.info->key         = 'i' ;
    flag.info->description = _("Report information about input raster and exit");
    
    flag.z_geometry = G_define_flag() ;
    flag.z_geometry->key         = 'd' ;
    flag.z_geometry->description = _("Generate vector points as 3D points");

    if (G_parser(argc, argv) != 0)
        exit(EXIT_FAILURE);
    
    /* please, remove before GRASS 7 released */
    if(flag.quiet->answer) {
        putenv("GRASS_VERBOSE=0");
        G_warning(_("The '-q' flag is superseded and will be removed "
            "in future. Please use '--quiet' instead."));
    }

    /* Set some state variables */
    myState.use_nulls = flag.zero->answer;
    myState.inraster  = parm.input->answer;
    myState.outraster = parm.raster->answer;
    myState.outsites  = parm.sites->answer;
    myState.z_geometry  = flag.z_geometry->answer;
    
    myState.mapset = G_find_cell (myState.inraster, "");
    if (myState.mapset == NULL)
    {
	G_fatal_error ("%s: <%s> raster map not found",
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
            G_fatal_error( _("%s: <%s> illegal file name"),
                G_program_name(), myState.outraster);
            exit(1);
        }
    }

    if (myState.outsites)
    {
        if (G_legal_filename (myState.outsites) < 0)
        {
            G_fatal_error( "%s: <%s> illegal file name",
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
