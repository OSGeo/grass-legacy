#include <stdlib.h>
#include <stdio.h>
#include "defs.h"
#include "gis.h"
#include "glocale.h"

int 
main (int argc, char *argv[])
{
    extern void parse();
    extern void find_edge_cells();
    extern void report();
    extern void read_labels();
    struct Parms parms;
    struct GModule *module;

    G_gisinit (argv[0]);
    
    /* Set description */
    module              = G_define_module();
    module->description = 
    _("Locates the closest points between objects in two raster maps.");

    parse (argc, argv, &parms);
    if (parms.labels)
    {
	read_labels (&parms.map1);
	read_labels (&parms.map2);
    }

    find_edge_cells (&parms.map1, parms.verbose);
    find_edge_cells (&parms.map2, parms.verbose);

    report (&parms);

    return 0;
}
