/*
 * Identify a color that has been set in the reset_color() (found in Reset_clr.c
 * file in this directory).  Subsequent graphics calls will use this color.
 *
 * Called by:
 *      Color() in ../lib/Color.c
 */

#include "iris.h"
/*DEBUG*/  
#include <stdio.h>

#ifdef color
#undef color
#endif

Dcolor (number)
	int number ;
{
    color (number + COLOR_OFFSET);
}
