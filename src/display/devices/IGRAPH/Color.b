head     1.1;
branch   ;
access   ;
symbols  ;
locks    ; strict;
comment  @ * @;


1.1
date     90.06.21.12.10.17;  author grass;  state Exp;
branches ;
next     ;


desc
@@



1.1
log
@Initial revision
@
text
@/*
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

color(number)
	int number ;
{
	/*  check range  */
	if ( number < 0 || number >= NCOLORS)
	{
		fgcolor( WNO, (unsigned long)WHITE) ;
		return(-1) ;
	}

	fgcolor( WNO, (unsigned long)number) ;
	return(0) ;
}
@
