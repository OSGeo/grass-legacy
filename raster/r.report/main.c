#define GLOBAL
#include "global.h"

int 
main (int argc, char *argv[])
{
    G_gisinit(argv[0]);
    G_get_window (&window);

    parse_command_line (argc, argv);

    get_stats();

    report();

    exit(0);
}
