#define global
#include "glob.h"

static struct parms parms;

main (argc, argv) char *argv[];
{

    G_gisinit (argv[0]);

    parse_command_line (argc, argv, &parms);

    if (parms.region)
	set_region();

    init_attributes (parms.fields);
    init_barriers (parms.barriers);

    read_point_list (parms.input, parms.fs);

    triangulate_point_list();

    break_connections();

    write_results (parms.output);

    exit(0);
}

be_quiet()
{
    return parms.quiet;
}
