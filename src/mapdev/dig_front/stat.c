#include "digit.h"
main (argc, argv)
    char *argv[];
{
    struct Map_info Map;

    if (argc != 2)
	fprintf (stderr, "Usage: %s vector-file\n", argv[0]), exit (-1);

    dig_P_init (argv[1], G_mapset (), &Map);

    printf ( "Lines: %d\n", Map.n_lines);
    printf ( "Nodes: %d\n", Map.n_nodes);
    printf ( "Areas: %d (%s)\n", Map.n_areas, Map.all_areas ? "complete" : "incomplete");
    printf ( "Isles: %d (%s)\n", Map.n_isles, Map.all_isles ? "complete" : "incomplete");
    printf ( "Atts: %d\n", Map.n_atts);
    dig_P_fini (&Map);
}
