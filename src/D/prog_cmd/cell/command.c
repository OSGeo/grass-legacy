#include "options.h"

set_default_options()
{
	overlay = 0 ;
	mapset = NULL ;
	strcpy(name, "") ;
}

stash_away(pos, option)
	int pos ;
	char *option ;
{
	switch(pos)
	{
	case NAME:
		strcpy (name, option);
		G_strip (name);
		if (*name == 0)
			return -1;
		break ;
	case OVER:
		if (! sscanf(option,"%d",&overlay) )
			return(-1) ;
		break ;
	default:
		printf("Unknown option\n") ;
		return(-1) ;
	}
	return(0) ;
}
