/*  %W%  %G%  */

#include "options.h"

set_default_options()
{
	strcpy(color,"white") ;
	size = 10000.0 ;
}

stash_away(pos, option)
	int pos ;
	char *option ;
{
	/* int new_colr ; */

	switch(pos)
	{
	case SIZE:
		if (! sscanf(option,"%d",&size) )
			return(-1) ;
		break ;
	case COLOR:
		strcpy(color, option);
		break ;
	default:
		printf("Unknown option\n") ;
		return(-1) ;
		break ;
	}
	return(0) ;
}
