#define GLOBAL
#include "global.h"

int 
main (int argc, char *argv[])
{
	struct GModule *module;

    G_gisinit(argv[0]);

	module = G_define_module();
	module->description =
		"Reports statistics for raster map layers.";

    G_get_window (&window);

    parse_command_line (argc, argv);

    get_stats();

    report();

    exit(0);
}
