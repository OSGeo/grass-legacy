/*  %W%  %G%  */

#include "options.h"

set_default_options()
{
	strcpy(map_name, "") ;
	strcpy(color, "white") ;
	size = 10.0 ;
	type = NORMAL ;
}

stash_away(pos, option)
	int pos ;
	char *option ;
{
	int new_color ;

	switch(pos)
	{
	case NAME:
		if (! sscanf(option,"%s",map_name) )
			return(-1) ;
		break ;
	case COLOR:
		strcpy(color, option) ;
		break ;
	case SIZE:
		if (! sscanf(option,"%f",&size) )
			return(-1) ;
		break ;
	case TYPE:
		if (strcmp(option,"fancy"))
			return(-1) ;
		type = FANCY ;
		break ;
	default:
		printf("Unknown option\n") ;
		return(-1) ;
		break ;
	}
	return(0) ;
}
