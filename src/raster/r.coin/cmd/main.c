#define GLOBAL
#include "coin.h"


main(argc, argv) char *argv[];
{
fill = "                                                                                                                                       ";
midline = "------------------------------------------------------------------------------------------------------------------------------------";

    G_gisinit (argv[0]);
    G_get_window(&window);

    dumpname = G_tempfile () ;
    statname = G_tempfile () ;

    window_cells = G_window_rows() * G_window_cols();


    if (argc > 1)
	command_version(argc, argv);
    else
	interactive_version();

    unlink (dumpname);
    unlink (statname);
    exit(0);
}
