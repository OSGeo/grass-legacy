/*  %W%  %G%  */

/*
 *   Xlabel
 *
 *   Usage:  Xlabel [textcolor] [backcolor] [size] [font]
 *           Xlabel [textcolor=name] [backcolor=name] [size=num] [font=name]
 *
 *   Draw interactive labels in a text window.
 */

#include <stdio.h>
#define MAIN
#include "options.h"
#define USAGE   "[textcolor=name] [backcolor=name] [size=num] [font=name]"

main(argc, argv)
int argc;
char **argv;
{
    extern int stash_away();

	/* Initialize the GIS calls */
    G_gisinit("Xlabel");

	/* Check command line */
    set_default_options();

    if (D_parse_command(argc, argv, variables, n_variables, stash_away)) {
        fprintf(stderr, "Usage: %s %s\n", argv[0], USAGE);
        exit(-1);
    }
    label();
}

