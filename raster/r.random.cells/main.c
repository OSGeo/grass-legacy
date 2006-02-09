/* main.c
 *
 * Generates random cell values with spatial dependence. (right?)
 *
 * AUTHOR: Charles Ehlschlaeger; National Center for Geographic Information
 * and Analysis, University of California, Santa Barbara.
*/

#undef TRACE
#undef DEBUG

#define MAIN
#include <grass/gis.h>
#include <grass/glocale.h>
#include "ransurf.h"
#include "local_proto.h"
#undef MAIN

int
main (int argc, char *argv[])
{
	struct GModule *module;
	
	FUNCTION(main);

	G_gisinit( argv[0]);
	/* Set description */
	module              = G_define_module();
	module->description = 
	_("Generates random cell values with spatial dependence.");
	
	Init( argc, argv);
	Indep();
	return 0;
}
