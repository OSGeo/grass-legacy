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

#define TOP_FUDGE 8
#define BOT_FUDGE 8
#define LEF_FUDGE 4
#define RIT_FUDGE 4

stash_away(pos, option)
	int pos ;
	char *option ;
{
	switch(pos)
	{
	case TOP:
		if (! sscanf(option,"%f",&top) )
			return(-1) ;
		top = top/100. * ((100-TOP_FUDGE)-(BOT_FUDGE)) + BOT_FUDGE;
		printf ( "TOP = %f\n", top);
		break ;
	case BOTTOM:
		if (! sscanf(option,"%f",&bottom) )
			return(-1) ;
		bottom = bottom/100.*((100-TOP_FUDGE)-(BOT_FUDGE)) + BOT_FUDGE;
		printf ( "BOTOM = %f\n", bottom);
		break ;
	case RIGHT:
		if (! sscanf(option,"%f",&right) )
			return(-1) ;
		right = right/100.*((100-RIT_FUDGE)-(LEF_FUDGE)) + LEF_FUDGE;
		printf ( "RIGHT = %f\n", right);
		break ;
	case LEFT:
		if (! sscanf(option,"%f",&left) )
			return(-1) ;
		left = left/100.*((100-RIT_FUDGE)-(LEF_FUDGE)) + LEF_FUDGE;
		printf ( "LEFT = %f\n", left);
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
