/* @(#)command.c	2.1   6/26/87 */

#include "options.h"

set_default_options()
{
	off = -1 ;
}

stash_away(pos, option)
	int pos ;
	char *option ;
{
	switch(pos)
	{
	case OFF:
		if (! sscanf(option,"%d",&off) )
			return(-1) ;
		break ;
	default:
		printf("Unknown option\n") ;
		return(-1) ;
		break ;
	}
	return(0) ;
}
