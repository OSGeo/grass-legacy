#define GLOBAL
#include "global.h"
#include "gis.h"
#include "glocale.h"

int 
main (int argc, char *argv[])
{
	struct GModule *module;

    G_gisinit(argv[0]);

	module = G_define_module();
	module->description =
		_("Reports statistics for raster map layers.");

    parse_command_line (argc, argv);

    G_get_window (&window);

    get_stats();

    report();

    exit(0);
}
