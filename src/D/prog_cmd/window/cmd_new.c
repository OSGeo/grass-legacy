/*  %W%  %G%  */

#include "opt_new.h"

set_default_options()
{
	top = 0 ;
	bottom = 0 ;
	left = 0 ;
	right = 0 ;
	strcpy(name, "") ;
}

stash_away(pos, option)
	int pos ;
	char *option ;
{
	switch(pos)
	{
	case TOP:
		if (! sscanf(option,"%f",&top) )
			return(-1) ;
		break ;
	case BOTTOM:
		if (! sscanf(option,"%f",&bottom) )
			return(-1) ;
		break ;
	case RIGHT:
		if (! sscanf(option,"%f",&right) )
			return(-1) ;
		break ;
	case LEFT:
		if (! sscanf(option,"%f",&left) )
			return(-1) ;
		break ;
	case NAME:
		strcpy(name, option) ;
		break ;
	default:
		printf("Unknown option\n") ;
		return(-1) ;
		break ;
	}
	return(0) ;
}
