/* @(#)colormode.c	2.1   6/26/87 */

/*
 *   Dcolormode
 *
 *   Usage:  Dcolormode mode=fixed/float
 *           Dcolormode offset=num
 *
 */

#define USAGE1	"fixed/float"

#include "gis.h"
#define MAIN
#include "options.h"

main(argc, argv)
	int argc ;
	char **argv ;
{
	extern int stash_away() ;

/* Check command line */
	set_default_options() ;

	if (D_parse_command(argc, argv, variables, n_variables, stash_away))
	{
		fprintf(stderr,"Usage: %s %s\n", argv[0], USAGE1) ;
		exit(-1) ;
	}

	R_open_driver();

	switch (mode)
	{
	case FLOAT:
		if (R_color_table_float())
			printf("Sorry, floating color table not available on this device\n") ;
		break ;
	case FIXED:
		if (R_color_table_fixed())
			printf("Sorry, fixed color table not available on this device\n") ;
		break ;
	default:
		break ;
	}

	R_close_driver();
}
