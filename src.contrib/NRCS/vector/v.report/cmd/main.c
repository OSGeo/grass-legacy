#define GLOBAL
#include "global.h"

main(argc, argv) char *argv[];
{
    G_gisinit(argv[0]);

    G_get_window (&window);

    /* RLG, 8/27/1997, for folks who thing lat/long maps can be measured */
    if(window.proj == PROJECTION_LL) {
          fprintf(stderr,"\nLat/Long projections are not supported.\n\n");
          exit(0);
    }
    parse_command_line (argc, argv);

    get_stats();

    report();

    exit(0);
}
