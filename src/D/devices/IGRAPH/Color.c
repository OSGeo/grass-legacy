/*
 * Identify a color that has been set in the reset_color() (found in Reset_clr.c
 * file in this directory).  Subsequent graphics calls will use this color.
 *
 * Called by:
 *      Color() in ../lib/Color.c
 *
 *  Written by the GRASS Team in the Winter of 88.
 *
 */

#include  "colors.h"

extern  int  WNO ;
extern  int  NCOLORS ;

int  last_color = 0 ;

color(number)
	int number ;
{
	/*  check range  */
	if ( number < 0 || number >= NCOLORS)
	{
		fgcolor( WNO, (unsigned long)WHITE) ;
		return(-1) ;
	}

	last_color = number ;
	fgcolor( WNO, (unsigned long)number) ;
	return(0) ;
}

Return_last_color()
{
	return(last_color) ;
}
