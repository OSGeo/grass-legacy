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
	struct GModule *module;
	int	DoMap, DoFilter, MapSeed;
	double	ran1();
	FUNCTION(main);

	G_gisinit( argv[0]);

	module = G_define_module();
	module->description =
		"Generates random surface(s) with spatial dependence.";

	Init( argc, argv);
	if( Uniform->answer)
		GenNorm();
	CalcSD();
	for( DoMap = 0; DoMap < NumMaps; DoMap++) {
		OutFD = G_open_cell_new ( OutNames[ DoMap]);
        	if( OutFD < 0) {
                	sprintf( Buf, 
				"%s: unable to open [%s] random raster map",
                        	G_program_name(), OutNames[ DoMap]);
                	G_fatal_error (Buf);
        	}
		if(! Verbose->answer)
			printf( "\nStarting map [%s]\n", OutNames[DoMap]);
		if( Seeds[DoMap] == SEED_MIN - 1)
			Seeds[ DoMap] = (int) (ran1() * SEED_MAX);
		MapSeed = Seed = Seeds[DoMap];
		ZeroMapCells();
		for( DoFilter = 0; DoFilter < NumFilters; DoFilter++) {
		    CopyFilter( &Filter, AllFilters[DoFilter]);
		    if(! Verbose->answer) {
			printf(
    "\nStarting filter #%d, distance: %.*lf, exponent: %.*lf, flat: %.*lf",
			DoFilter,
			Digits( 2.0 * Filter.MaxDist, 6),
			2.0 * Filter.MaxDist,
			Digits( 1.0 / Filter.Exp, 6),
			1.0 / Filter.Exp,
			Digits( Filter.Mult, 6),
			Filter.Mult);
			printf( "\nPercent done:");
		    }
		    MakeBigF();
		    CalcSurface();
		}
		if(! Verbose->answer) printf( "\n");
		SaveMap( DoMap, MapSeed);
	}
}
