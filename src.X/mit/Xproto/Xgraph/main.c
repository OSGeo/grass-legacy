
/*
 *   Xgraph
 *   Usage:  Xgraph [input] [color] [hsize] [vsize]
 *           Xgraph [input=name] [color=name] [hsize=num] [vsize=num]
 *   Draw graphics in a graphics window.   Graph lines come from stdin,
 *      unless input specified.
 */

#define MAIN
#include "options.h"
#include <stdio.h>
#define USAGE   "[input=name] [color=name] [hsize=num] [vsize=num]"

main(argc, argv)
int argc;
char **argv;
{
    extern int stash_away();

	/* Initialize the GIS calls */
    G_gisinit(argv[0]);

	/* Check command line */
    set_default_options();

    if (D_parse_command(argc, argv, variables, n_variables, stash_away)) {
        fprintf(stderr, "Usage: %s %s\n", argv[0], USAGE);
        exit(-1);
    }
	/* Do the graphics */
    graphics(stdin);
}

