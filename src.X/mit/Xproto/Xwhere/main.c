/*  %W%  %G%  */
/*
 *   Xwhere
 *   Usage:  Xwhere [s=spheroid] [m=mode]
 */

#define MAIN
#include "gis.h"
#include "options.h"

struct Cell_head window;

main(argc, argv)
int argc;
char **argv;
{
    int stash_away();

    /* Initialize the GIS calls */
    G_gisinit(argv[0]);

    mode = LOUD;
    have_spheroid = 0;
    if (D_parse_command(argc, argv, variables, n_variables, stash_away)) {
        fprintf(stderr, "Usage: %s [m=mode] [s=spheroid]\n", argv[0]);
        fprintf(stderr, "  mode 'quiet' specifies a non-prompted one-time event\n");
        fprintf(stderr, "  spheroid, if specified, prints lat/lon as well\n");
        fprintf(stderr, "\nKnown spheroids\n");
        list_spheroids();
        exit(-1);
    }

	/* Read in the map window associated with window */
    G_get_window(&window);

    where_am_i();

    fprintf(stderr, "\n");
}

