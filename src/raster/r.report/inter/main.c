#define GLOBAL
#include "global.h"
main(argc, argv) char *argv[];
{
    G_gisinit(argv[0]);

    stats_file = G_tempfile();
    report_file = G_tempfile();

    ask_layers();

    run_report(0);

    while(ask_units())
	run_report(1);
    
    unlink (stats_file);
    unlink (report_file);
    exit(0);
}
