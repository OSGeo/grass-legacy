/*  %W%  %G%  */

#include "opt_which.h"

set_default_options()
{
	X = 0. ;
	Y = 0. ;
}

stash_away(pos, option)
	int pos ;
	char *option ;
{
	switch(pos)
	{
	case HORI:
		if (! sscanf(option,"%f",&X) )
			return(-1) ;
		break ;
	case VERT:
		if (! sscanf(option,"%f",&Y) )
			return(-1) ;
		Y = 100.0 - Y ;
		break ;
	default:
		printf("Unknown option\n") ;
		return(-1) ;
		break ;
	}
	return(0) ;
}
