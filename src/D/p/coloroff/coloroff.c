/* @(#)coloroff.c	2.1   6/26/87 */

/*
 *   Dcoloroff
 *
 *   Usage:  Dcoloroff offset=num
 *
 */

#define USAGE	"offset=number"

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
		fprintf(stderr,"Usage: %s %s\n", argv[0], USAGE) ;
		exit(-1) ;
	}

	R_open_driver();

	if (off >= 0)
		D_claim_offset_is(off) ;

	R_close_driver();
}
