/* main.c
 *
 * Generates random cell values with spatial dependence. (right?)
 *
 * AUTHOR: Charles Ehlschlaeger; National Center for Geographic Information
 * and Analysis, University of California, Santa Barbara.
*/

#define TRACE
#undef TRACE
#define DEBUG
#undef DEBUG

#define MAIN
#include "ransurf.h"
#undef MAIN

int
main (int argc, char *argv[])
{
	int	DoMap, DoFilter, MapSeed;
	double	ran1();
	struct GModule *module;
	
	FUNCTION(main);

	G_gisinit( argv[0]);
	/* Set description */
	module              = G_define_module();
	module->description = ""\
	"Generates random cell values with spatial dependence.";
	
	Init( argc, argv);
	Indep();
}
