/*
 * Identify a color that has been set in the reset_color() (found in Reset_clr.c
 * file in this directory).  Subsequent graphics calls will use this color.
 *
 * Called by:
 *      Color() in ../lib/Color.c
 */

#include <grass/gis.h>
#include <grass/colors.h>
#include "pngdriver.h"

void PNG_color(int number)
{
	if (number >= NCOLORS || number < 0)
	{
		G_warning("Color: can't set color %d\n", number);
		return;
	}

	currentColor = number;
}

int PNG_get_color(void)
{
	return currentColor;
}

