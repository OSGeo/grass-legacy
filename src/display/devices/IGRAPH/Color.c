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

#include <stdio.h>
#include  "colors.h"
#include "igraphics.h"

extern  int  WNO ;
extern  int  NCOLORS ;
extern int I_COLOR_OFFSET ;

int  last_color = 0 ;

color(number)
	int number ;
{

    /*set slot to skip over IGRAPH fixed colors--dks*/
	number += I_COLOR_OFFSET;

/*DEBUG*/ /* fprintf (stderr, "color:%d", number); */
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
