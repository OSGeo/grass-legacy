#define     USAGE   "cell_file"

#define MAIN
#include "options.h"
#include "gis.h"

main(argc, argv)
int argc;
char *argv[];
{
    char buf[120];
    char *mapset;
    extern int stash_away();


    /* Initialize the GIS calls */
    G_gisinit("Xcell");

    /* Check command line */
    set_default_options();

    if (D_parse_command(argc, argv, variables, n_variables,
            stash_away)) {
        fprintf(stderr, "Usage: %s %s\n", argv[0], USAGE);
        exit(-1);
    }
    /* Make sure map is available */
    mapset = G_find_cell2(name, "");

    if (mapset == NULL) {
        sprintf(buf, "Cellfile [%s] not available", name);
        G_fatal_error(buf);
    }
    Xcell(name, mapset, overlay);
    exit(0);
}
