/* main.c								*/

#define TRACE
#undef TRACE
#define DEBUG
#undef DEBUG

#define MAIN
#include "ransurf.h"
#undef MAIN

main (argc, argv)
	int	argc;
	char    *argv[];
{
	int	DoMap, DoFilter, MapSeed;
	double	ran1();
	FUNCTION(main);

	G_gisinit( argv[0]);
	Init( argc, argv);
	Indep();
}
