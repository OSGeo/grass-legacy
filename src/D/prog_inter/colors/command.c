#include "options.h"

set_default_options()
{
	strcpy(map_name, "") ;
}

stash_away(pos, option)
	int pos ;
	char *option ;
{
	int new_colr ;

	switch(pos)
	{
	case NAME:
		strcpy (map_name, option);
		G_strip (map_name);
		if (*map_name == 0)
			return -1;
		break ;
	default:
		printf("Unknown option\n") ;
		return(-1) ;
		break ;
	}
	return(0) ;
}
