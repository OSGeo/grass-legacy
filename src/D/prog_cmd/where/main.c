/*
 *   Dwhere
 *
 *   Usage:  Dwhere [s=spheroid] [m=mode]
 *
 */


#define MAIN
#include "gis.h"
#include "options.h"

main(argc, argv)
    int argc ;
    char **argv ;
{
    int stash_away();

/* Initialize the GIS calls */
    G_gisinit(argv[0]) ;

    mode = LOUD;
    have_spheroid = 0;
    if (D_parse_command(argc, argv, variables, n_variables, stash_away))
    {
	fprintf(stderr,"Usage: %s [m=mode] [s=spheroid]\n", argv[0]) ;
	fprintf(stderr, "  mode 'quiet' specifies a non-prompted one-time event\n");
	fprintf(stderr, "  spheroid, if specified, prints lat/lon as well\n");
	if (G_projection() != PROJECTION_UTM)
	{
	    fprintf (stderr, "  (this option implemented only for UTM databases)\n");
	}
	else
	{
	    fprintf(stderr, "\nKnown spheroids\n");
	    list_spheroids();
	}
	exit(-1) ;
    }
    R_open_driver();
    D_setup(0);

    where_am_i() ;

    fprintf(stderr,"\n") ;

    R_close_driver();
}
